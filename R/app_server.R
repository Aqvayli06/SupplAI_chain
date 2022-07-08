
#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  db_name <- "./inst/db_warehouse_mgmt.sqlite"
  # check_credentials directly on sqlite db
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = initiate_cred_db(),
      passphrase = keyring::key_get("R-keyring-test-service", "donaldduck")
      # passphrase = "passphrase_wihtout_keyring"
    )
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  



  
  output$costumer_bills_list_box <- renderUI({
    bs4Dash::box(title = "Costumer Bills",status = "lightblue",collapsible   = TRUE,width = 12,
                 DT::dataTableOutput("costumer_bill_list")
    )
  })
  
  ### Sales
  mod_sale_manager_server("mod_sale_manager_1")
  
  ### Orders
  mod_order_manager_server("order_manager_1")
  
  # Business Overview
  mod_business_overview_server("business_overview_1")
  
  # Sales Forecaster
  mod_sales_forecaster_server("sales_forecaster_1")
  
  # Inventories
  mod_inventory_manager_server("inventory_manager_1")
  
  # Replenishment Strategy
  mod_replenishment_strategy_server("replenishment_strategy_1")
}
