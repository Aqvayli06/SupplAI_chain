#' sales_forecaster UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sales_forecaster_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("sales_forecaster_param_box")),
    uiOutput(ns("sales_forecaster_box")),
    uiOutput(ns("sales_actuals_box"))
  )
}
    
#' sales_forecaster Server Functions
#'
#' @noRd 
mod_sales_forecaster_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # open connection to database
    db_con <- open_warehouse_db()
    sc_meta_data <- load_db_metadata()
    date_var    <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="date")]
    item_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="item_ID")]
    store_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="store_ID")]
    target_value_vars <- "sales"
    
    
    # load sales historical data from database
    sales_data_raw <- reactive({
      sales_data  <- load_data_from_database(target_table = "sales_table",db_con = db_con)
      date_vect   <- sales_data%>%dplyr::pull(!!date_var)%>%lubridate::as_date(., origin = lubridate::origin) # convert date into the right format
      sales_data  <- sales_data%>%dplyr::mutate(!!date_var := date_vect)
      return(sales_data)  
    })
    
    output$target_item_ID <- renderUI({
      req(sales_data_raw())
      item_ID_choices <- sales_data_raw()%>%dplyr::pull(!!item_ID_field)%>%unique()
      selectInput(inputId = ns("target_item_ID"),label = item_ID_field, choices = item_ID_choices, multiple = TRUE)
    })
    
    
    output$target_store_ID <- renderUI({
      req(sales_data_raw())
      store_ID_choices <- sales_data_raw()%>%dplyr::pull(!!store_ID_field)%>%unique()
      selectInput(inputId = ns("target_store_ID"),label = store_ID_field, choices = store_ID_choices,multiple = TRUE)
    })
    
    output$target_time_period <- renderUI({
      req(sales_data_raw())
      start_date <- sales_data_raw()%>%dplyr::pull(!!date_var)%>%min()
      end_date <- sales_data_raw()%>%dplyr::pull(!!date_var)%>%max()
      dateRangeInput(inputId = ns("target_time_period"),label = "Time Period",start = start_date, end = end_date,
                     min = start_date,
                     max = end_date)
    })
    
    output$sales_forecaster_param_box <- renderUI({
      bs4Dash::box(title = "Parameters",status = "success",collapsible   = TRUE,width = 12,
                   fluidRow(
                     col_3(uiOutput(ns("target_item_ID"))),
                     col_3(uiOutput(ns("target_store_ID"))),
                     col_2(uiOutput(ns("target_time_period"))),
                     col_2(uiOutput(ns("forecast_horizon"))),
                     col_2(uiOutput(ns("generate_sales_forecast"))),
                     col_2(uiOutput(ns("upload_sales_forecasts_to_db")))
                     
                   )
      )# box sales_forecaster params
    })# sales_forecaster_param
    
    sales_actuals <- reactive({
      req(input$target_item_ID)
      req(input$target_store_ID)
      sales_actuals <- sales_data_raw()%>%dplyr::filter(.data[[item_ID_field]] %in% !! input$target_item_ID &
                                                          .data[[store_ID_field]] %in% !! input$target_store_ID &
                                                          .data[[date_var]] >= !!input$target_time_period[1] & 
                                                          .data[[date_var]] <= !!input$target_time_period[2] )%>%
        tidyr::pivot_wider(
          id_cols = !!date_var,
          
          names_from = c(!!item_ID_field,!!store_ID_field),
          values_from = !!target_value_vars ,
          names_sep = "+" # VERY IMPORTANT(There should be not + in item_ID and store_ID)
        )
      warning("There should NOT be + character  in item_ID and store_ID")
      return(sales_actuals)
    })
    
    
    output$sales_actuals_DT <- DT::renderDataTable({
      sales_actuals()
    })
    
    
    output$forecast_horizon <- renderUI({
      req(sales_actuals())
      selectInput(inputId = ns("forecast_horizon"), label = "Forecast Horizon", choices = seq(1,31), selected = NULL)
    })
    
    output$generate_sales_forecast <- renderUI({
      actionButton(inputId = ns("generate_sales_forecast"), label = "Generate Forecast", icon = icon("fa-solid fa-chart-line"))
    })
    sales_forecasts <- eventReactive(input$generate_sales_forecast,{
      req(sales_actuals())
      target_variables <- colnames(sales_actuals())
      target_variables <- target_variables[target_variables != date_var]
      sales_forecasts <- sales_actuals()%>%SaldaeForecasting::Saldae_Forecaster(tisefka = ., target_variables = target_variables,anomaly_detection = TRUE)%>%
        purrr::map(.x = .,  ~ SaldaeForecasting::sbed_forecast_aqerru(.x , asurif_arzdat = input$forecast_horizon))
      return(sales_forecasts)
    })
    
    sales_fcast_plots <- reactive({
      req(sales_forecasts())
      plot_settings <- list()
      plot_settings[["colors_inu"]] <- c("lightblue","darkgreen","brown","#EBCC2A")
      purrr::map(.x =names(sales_forecasts()),~SaldaeForecasting::sekned_forecast_aqeru(fcast_df =  sales_forecasts()[[.x]],target_variable = .x ,plot_settings = plot_settings))%>%
        stats::setNames(names(sales_forecasts()))
    })
    
    output$sales_forecast_plot <- plotly::renderPlotly({
      req(sales_fcast_plots())
      sales_fcast_plots()[[1]]
    })
    output$sales_actuals_box <- renderUI({
      bs4Dash::box(title = "Sales Actuals",status = "info",collapsible   = TRUE,width = 12,
                   DT::dataTableOutput(ns("sales_actuals_DT"))
      )# box: sales_forecaster
    }) 
    output$sales_forecaster_box <- renderUI({
      bs4Dash::box(title = "Sales Forecasts",status = "danger",collapsible   = TRUE,width = 12,
                   plotly::plotlyOutput(ns("sales_forecast_plot"))
      )# box: sales_forecaster
    })
    
    output$upload_sales_forecasts_to_db <- renderUI({
     
      req(sales_forecasts())
      shiny::actionButton(inputId = ns("upload_sales_forecasts_to_db"), label = "Upload to Database", icon = icon("fa-solid fa-upload"))
    })
    observeEvent(input$upload_sales_forecasts_to_db,{
      sales_meta <- list(table_name = "Sales Forecast", date_format = "%Y-%m-%d")
      
      sales_forecasts()%>%
        prepare_forecast_for_db_upload(variables_to_drop = c("actuals","corrected"), sc_meta_data= sc_meta_data)%>%
        upload_to_database(input_data = .,upload_mode = "append", target_table = "sales_forecast_table", meta_data = sales_meta, meta_data_file = "./inst/sc_project_metadata.json")
      shinyalert::shinyalert(title = "Sales Forecast", text = "Successfully Uploaded", type = "success")
    })
    
  })#mod server
}
    
## To be copied in the UI
# mod_sales_forecaster_ui("sales_forecaster_1")
    
## To be copied in the server
# mod_sales_forecaster_server("sales_forecaster_1")
