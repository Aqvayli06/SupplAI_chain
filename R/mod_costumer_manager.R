#' costumer_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_costumer_manager_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("costumer_infos_box"))
  )
}
    
#' costumer_manager Server Functions
#'
#' @noRd 
mod_costumer_manager_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$costumer_firstname <- renderUI({
      textInput(inputId = ns("costumer_firstname"),label = "Nom du client",value = "")
    })
    output$costumer_lastname <- renderUI({

      textInput(inputId = ns("costumer_lastname"),label = "lastname du client",value = "")
    })
    output$costumer_entreprise <- renderUI({
      textInput(inputId = ns("costumer_entreprise"),label = "Nom Entreprise",value = "")
    })
    output$costumer_discount <- renderUI({
      numericInput(inputId = ns("costumer_discount"),
                   label = "Discount  €",
                   value = 0)
    })
    output$costumer_infos_box <- renderUI({
      bs4Dash::box(title = "Costumer",status = "gray",collapsible   = TRUE,width = 12,
                   fluidRow(
                     col_3(uiOutput(ns("costumer_firstname"))),
                     col_3(uiOutput(ns("costumer_lastname"))),
                     col_3(uiOutput(ns("costumer_entreprise"))),
                     col_3(uiOutput(ns("costumer_discount")))
                   )
      ) # box : costumer_infos_box 
      
    })
  })
}
    
## To be copied in the UI
# mod_costumer_manager_ui("costumer_manager_1")
    
## To be copied in the server
# mod_costumer_manager_server("costumer_manager_1")
