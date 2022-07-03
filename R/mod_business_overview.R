#' business_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_business_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("data_overview_param_box")),
    uiOutput(ns("data_overview_box"))
  )
}
    
#' business_overview Server Functions
#'
#' @noRd 
mod_business_overview_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    db_con <- open_warehouse_db()
    sc_meta_data <- load_db_metadata()
    output$target_overview_data <- renderUI({
      overview_choices <- sc_meta_data$db_tables
      names(overview_choices) <- sc_meta_data$db_tables_names
      selectInput(inputId = ns("target_overview_data"),label = "Target Data", choices = overview_choices)
    })
    
    output$target_overview_startdate <- renderUI({
      dateInput(inputId = ns("target_overview_startdate"),label = "Start Date")
    })
    
    output$target_overview_enddate <- renderUI({
      dateInput(inputId = ns("target_overview_enddate"),label = "End Date")
    })
    
    overview_data <- reactive({
      req(input$target_overview_data)
      load_data_from_database(target_table = input$target_overview_data,db_con = db_con)
    })
    output$overview_data_DT <- DT::renderDataTable({
      overview_data()
    })
    
    output$data_overview_param_box <- renderUI({
      bs4Dash::box(title = "Parameters",status = "success",collapsible   = TRUE,width = 12,
                   fluidRow(
                     col_3(uiOutput(ns("target_overview_data"))),
                     col_3(uiOutput(ns("target_overview_startdate"))),
                     col_3(uiOutput(ns("target_overview_enddate"))),
                   )
                   )# box data overview params
    })# data_overview_param
    output$data_overview_box <- renderUI({
      bs4Dash::box(title = "Output",status = "info",collapsible   = TRUE,width = 12,
                   DT::dataTableOutput(ns("overview_data_DT"))
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_business_overview_ui("business_overview_1")
    
## To be copied in the server
# mod_business_overview_server("business_overview_1")
