#' replenishment_strategy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_replenishment_strategy_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("replenishment_strategy_box"))
  )
}
    
#' replenishment_strategy Server Functions
#'
#' @noRd 
mod_replenishment_strategy_server <- function(id){
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
      selectInput(inputId = ns("target_store_ID"),label = store_ID_field, choices = store_ID_choices)
    })
    
    replenishment_strategy_parameters <- reactive({
      req(input$target_item_ID)
      req(input$target_store_ID)
      temp_params <- load_replenishemnt_parameters(db_con = db_con , item_ID = input$target_item_ID,
                                    store_ID = input$target_store_ID, sc_meta_data = sc_meta_data)
      if(nrow(temp_params)==0){
        temp_params <- data.frame(safety_stock =  3, forecast_horizon = 15, supply_frequency = 4)
      }
      return(temp_params)
    }) # replenishment_strategy_parameters
    
    
    output$forecast_horizon <- renderUI({
      req(available_item_stores())
      forecast_horizons <- 4:90
      selectInput(inputId = ns("forecast_horizon"), 
                  label = "Forecast Horizon", choices = forecast_horizons,
                  selected = replenishment_strategy_parameters()$forecast_horizon)
    })
    output$safety_stock <- renderUI({
      req(available_item_stores())
      safety_stocks <- 1:90
      selectInput(inputId = ns("safety_stock"), label = "Safety Stock",choices = safety_stocks,
                  selected = replenishment_strategy_parameters()$safety_stock)
    })
    
    output$supply_frequency <- renderUI({
      req(available_item_stores())
      supply_frequencies <- 1:10
      selectInput(inputId = ns("supply_frequency"), label = "Supply Frequency",choices = supply_frequencies,
                  selected = replenishment_strategy_parameters()$supply_frequency)
    })
    
    output$lead_time <- renderUI({
      req(available_item_stores())
      lead_times <- 1:10
      selectInput(inputId = ns("lead_time"), label = "Lead Time",choices = lead_times,
                  selected = replenishment_strategy_parameters()$lead_time)
    })
    
    output$update_replenishment_params <- renderUI({
      actionButton(inputId = ns("update_replenishment_params"),label = "Update Params")
    })
    
    observeEvent(input$update_replenishment_params,{
      update_replenishment_params_table(db_con = db_con , target_item_ID = input$target_item_ID,
                                        target_store_ID = input$target_store_ID,
                                        forecast_horizon = input$forecast_horizon,
                                        safety_stock = input$safety_stock,
                                        supply_frequency = input$supply_frequency)
      shinyalert::shinyalert("Replenishment", text = "Parameters Updated",type = "success")
    })
    
    output$replenishment_strategy_box <- renderUI({
      bs4Dash::box(title = "Input Parameters",status = "success",collapsible   = TRUE,width = 12,
                   fluidRow(
                     col_2(uiOutput(ns("target_item_ID"))),
                     col_2(uiOutput(ns("target_store_ID"))),
                     col_2(uiOutput(ns("forecast_horizon"))),
                     col_2(uiOutput(ns("safety_stock"))),
                     col_2(uiOutput(ns("supply_frequency"))),
                     col_2(uiOutput(ns("lead_time"))),
                     col_2(uiOutput(ns("update_replenishment_params")))
                   )
      )# replenishment_strategy_box
    })# replenishment_strategy
 
  })
}
    
## To be copied in the UI
# mod_replenishment_strategy_ui("replenishment_strategy_1")
    
## To be copied in the server
# mod_replenishment_strategy_server("replenishment_strategy_1")
