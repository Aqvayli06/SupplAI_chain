#' inventory_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_inventory_manager_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("sales_forecaster_param_box")),
    fluidRow(
      col_6(uiOutput(ns("demand_forecast_box"))),
      col_6(uiOutput(ns("strategy_params_box")))
    ),
    uiOutput(ns("projected_inventory_box"))
  )
}
    
#' inventory_manager Server Functions
#'
#' @noRd 
mod_inventory_manager_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # open connection to database
    db_con <- open_warehouse_db()
    sc_meta_data <- load_db_metadata()
    
    item_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="item_ID")]
    store_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="store_ID")]
    
    target_value_vars <- "sales"
    date_var    <- "date"
    
    
    available_item_stores <- reactive({
      load_sales_forecasts(db_con = db_con, target_attribute = paste(item_ID_field,store_ID_field,sep = ","))%>%unique()
    })
    
    output$target_item_ID <- renderUI({
      req(available_item_stores())
      item_ID_choices <- available_item_stores()%>%dplyr::pull(!!item_ID_field)%>%unique()
      selectInput(inputId = ns("target_item_ID"),label = item_ID_field, choices = item_ID_choices, multiple = FALSE)
    })
    
    output$target_store_ID <- renderUI({
      req(available_item_stores())
      store_ID_choices <- available_item_stores()%>%dplyr::pull(!!store_ID_field)%>%unique()
      selectInput(inputId = ns("target_store_ID"),label = store_ID_field, choices = store_ID_choices,multiple = FALSE)
    })
    
    output$sales_forecaster_param_box <- renderUI({
      bs4Dash::box(title = "Parameters",icon = icon("fa-solid fa-gears"),status = "success",collapsible   = TRUE,width = 12,
                   fluidRow(
                     col_3(uiOutput(ns("target_item_ID"))),
                     col_3(uiOutput(ns("target_store_ID")))
                   )
      )# box sales_forecaster params
    })# sales_forecaster_param
    
    replenishment_strategy_parameters <- reactive({
      req(input$target_item_ID)
      req(input$target_store_ID)
      load_replenishemnt_parameters(db_con = db_con , item_ID = input$target_item_ID,
                                    store_ID = input$target_store_ID, sc_meta_data = sc_meta_data)
    }) # replenishment_strategy_parameters
    
    sales_forecast <- reactive({
      req(input$target_item_ID)
      req(input$target_store_ID)
      load_sales_forecasts(db_con = db_con, item_IDs = input$target_item_ID, 
                           store_IDs = input$target_store_ID,as_of_date = NULL,sc_meta_data = sc_meta_data,
                           date_freq = NULL,fcast_horizon = replenishment_strategy_parameters()$forecast_horizon)
    })
    
    output$demand_forecast_plot <- plotly::renderPlotly({
      req(sales_forecast())
      sales_forecast()%>%dplyr::group_by(as_of_date,date)%>%
        dplyr::summarise_at(c("forecast",  "upper",   "lower"),sum)%>%
        generate_sales_forecast_chart(target_variable = "Demand(u)")
    })
    
    # demand forecast 
    output$demand_forecast_box <- renderUI({
      bs4Dash::box(title = "Demand Forecast",icon = icon("fa-solid fa-basket-shopping"),status = "danger",collapsible   = TRUE ,width = 12,
                   plotly::plotlyOutput(ns("demand_forecast_plot"))
      )# box: demand_forecast
    })
    
    
    # Strategy Params 
    output$params_safety_stock <- bs4Dash::renderInfoBox({
      bs4Dash::infoBox(
        title = "Safety Stock",
        value = replenishment_strategy_parameters()$safety_stock,
        subtitle = NULL,
        icon = shiny::icon("fa-solid fa-shield"),
        color = "secondary",
        width = 4)
    })
    output$params_supply_freq <- bs4Dash::renderInfoBox({
      bs4Dash::infoBox(
        title = "Supply Frequency",
        value = replenishment_strategy_parameters()$supply_freq,
        subtitle = NULL,
        icon = shiny::icon("fa-solid fa-truck"),
        color = "warning",
        width = 4)
    })
    
    output$params_projected_horizon <- bs4Dash::renderInfoBox({
      bs4Dash::infoBox(
        title = "Projection Horizon",
        value = replenishment_strategy_parameters()$forecast_horizon,
        subtitle = NULL,
        icon = shiny::icon("fa-solid fa-arrow-right"),
        color = "lightblue",
        width = 4)
    })
    
    output$params_lead_time <- bs4Dash::renderInfoBox({
      bs4Dash::infoBox(
        title = "Lead Time(days)",
        value = replenishment_strategy_parameters()$lead_time,
        subtitle = NULL,
        icon = shiny::icon("fa-solid fa-hourglass-start"),
        color = "navy",
        width = 4)
    })
    
    output$strategy_params_box <- renderUI({
      bs4Dash::box(title = "Strategy",status = "info",collapsible   = TRUE ,width = 12,
                   uiOutput(ns("params_safety_stock")),
                   uiOutput(ns("params_supply_freq")),
                   uiOutput(ns("params_projected_horizon")),
                   uiOutput(ns("params_lead_time"))
                   
      )
    })# box : strategy_params_box
    
    # projected inventory
    projected_inventory <- reactive({
      req(sales_forecast())
      req(replenishment_strategy_parameters())
      sales_forecast()%>%
        calculate_projected_inventory_main(
          safety_stock = replenishment_strategy_parameters()$safety_stock, 
          supply_freq = replenishment_strategy_parameters()$supply_freq,
          projection_horizon = replenishment_strategy_parameters()$forecast_horizon)
    })
    output$projected_inventory_DT <- DT::renderDataTable({
      grad_col_var <- c("demand_fcast","drp_amount","projected_inventory")
      names(grad_col_var) <- c("#1B9E77","#D95F02","#E6AB02")
      projected_inventory()%>%
        display_dt_table_DT(grad_col_var = grad_col_var)
    })
    output$projected_inventory_chart <- plotly::renderPlotly({
      projected_inventory()%>%
        generate_projected_inventory_chart()
    })
    
    output$projected_inventory_box <- renderUI({
      bs4Dash::box(title = "Projected Inventory",icon = icon("fa-solid fa-warehouse"),status = "warning",collapsible   = TRUE ,width = 12,
                   fluidRow(
                     col_6(plotly::plotlyOutput(ns("projected_inventory_chart"))),
                     col_6(DT::dataTableOutput(ns("projected_inventory_DT")))
                   )
      )# box: demand_forecast
    })
    
  })
}
    
## To be copied in the UI
# mod_inventory_manager_ui("inventory_manager_1")
    
## To be copied in the server
# mod_inventory_manager_server("inventory_manager_1")
