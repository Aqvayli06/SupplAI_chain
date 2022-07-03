#' replenishment_simulation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_replenishment_simulation_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' replenishment_simulation Server Functions
#'
#' @noRd 
mod_replenishment_simulation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_replenishment_simulation_ui("replenishment_simulation_1")
    
## To be copied in the server
# mod_replenishment_simulation_server("replenishment_simulation_1")
