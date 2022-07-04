#' update_replenishment_params_table
#' @param db_con a database connection
#' @param target_item_ID the article 
#' @param target_store_ID target store ID
#' @param forecast_horizon in days / weeks / months
#' @param safety_stock in days/weeks/months
#' @param supply_frequency in days / week / months
#' @export

update_replenishment_params_table <- function(db_con = NULL , target_item_ID = NULL,target_store_ID = NULL,
                                  forecast_horizon = NULL, safety_stock = NULL, supply_frequency = NULL,delivery_delay = NULL){
  if(is.null(db_con))db_con <- open_warehouse_db()
    target_table <- "inventory_params_table"
    inventory_params <- data.frame(item_ID = target_item_ID,store_ID = target_store_ID,
                                   forecast_horizon = forecast_horizon,safety_stock = safety_stock,
                                   supply_frequency = supply_frequency, update_time = Sys.time(),user_ID = "t.b.d")
    
    inventory_params_meta   <- list(table_name = "Inventory Parameters", attributes_names = colnames(inventory_params), date_format = "%Y-%m-%d %H:%M:%S")
    
    inventory_params%>%
      upload_to_database(input_data = ., target_table = target_table ,upload_mode = "append", meta_data = inventory_params_meta)
  return(inventory_params)
}

#' load_replenishemnt_parameters
#' @param db_con database connection
#' @param item_ID item identifier
#' @export

load_replenishemnt_parameters <- function(db_con = NULL,item_ID = NULL, store_ID = NULL,sc_meta_data = NULL){
  
  item_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="item_ID")]
  store_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="store_ID")]
  
  # query_statement <- paste0("SELECT * FROM inventory_params_table WHERE ",item_ID_field ,"=='",item_ID, "' AND ","store_ID_field =='",store_ID, "'") 
  query_statement <- paste0("SELECT * FROM inventory_params_table WHERE  item_ID =='",item_ID, "' AND store_ID =='",store_ID, "'")
  
  replenishment_params <- odbc::dbGetQuery(conn = db_con,statement = query_statement)%>%
    tail(1)

  return(replenishment_params)
  # sales_forecasts <- sales_forecasts%>%
  #   dplyr::filter(.data[[item_ID_field]] %in% !! item_IDs &
  #                   .data[[store_ID_field]] %in% !! store_IDs)
}