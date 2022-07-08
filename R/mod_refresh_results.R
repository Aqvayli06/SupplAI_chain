#' refresh_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_refresh_results_ui <- function(id){
  ns <- NS(id)
  tagList(
   uiOutput(ns("refresh_results"))
  )
}
    
#' refresh_results Server Functions
#'
#' @noRd 
mod_refresh_results_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$refresh_results <- renderUI({
      bs4Dash::actionButton(ns("refresh_results"), label = "", icon = icon("fa-solid fa-rotate"))
    })
    # refresh results
    observeEvent(input$refresh_results, {
      shinyjs::refresh()
    })
  })
}
    
## To be copied in the UI
# mod_refresh_results_ui("refresh_results_1")
    
## To be copied in the server
# mod_refresh_results_server("refresh_results_1")
