#' item_search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_item_search_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' item_search Server Functions
#'
#' @noRd 
mod_item_search_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_item_search_ui("item_search_1")
    
## To be copied in the server
# mod_item_search_server("item_search_1")
