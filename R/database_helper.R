#' generate dummy data
#' @export

generate_dummay_data <- function(){
  dummy_list <- list()
  dummy_list[[1]] <- data.frame(item_name = "Torche" , item_ID ="1000", category = "Appareil Dentaire",marque = "Philips",
                            quantity_disponible = 120, purchase_price = 130, sell_price = 189, discount = 10, garantie = 36, poids = 2.5,fournisseur = "iVirtuals")
  
  dummy_list[[2]] <- data.frame(item_name = "Scanner" , item_ID ="0100", category = "Medical",marque = "SIEMENS",
                            quantity_disponible = 12, purchase_price = 130, sell_price = 189, discount = 10, garantie = 36, poids = 2.5,fournisseur = "iVirtuals")
  dummy_list[[3]] <- data.frame(item_name = "Appareil Tension" , item_ID ="0100", category = "Medical",marque = "iVirtual",
                                quantity_disponible = 12, purchase_price = 130, sell_price = 189, discount = 10, garantie = 36, poids = 2.5,fournisseur = "iVirtuals")
  dummy_list[[4]] <- data.frame(item_name = "Perceuse" , item_ID ="0100", category = "Technique",marque = "BOSCH",
                                quantity_disponible = 12, purchase_price = 130, sell_price = 189, discount = 10, garantie = 36, poids = 2.5,fournisseur = "iVirtuals")
  dummy_list <- dummy_list%>%do.call(rbind,.)
  return(dummy_list)
}

#'initialize warehouse database
#'@description create all data tables related to product , warehouse, stores, ....
#'@param db_con database connection
#'@export

initialize_warehouse_database <- function(db_con = NULL){
  product_table <- generate_dummay_data()
  #
  odbc::dbWriteTable(db_con , "product_table", product_table,overwrite = T)
  
  # create costumer table
  costumer_table <- data.frame(costumer_ID = "CST0001", costumer_first_name = "Farid",costumer_last_name = "Azouaou",costumer_tel = "xxxxxx",costumer_entreprise = "myCompany", first_registration = Sys.Date())
  
  # odbc::sqlCreateTable(db_con , "costumer_table", costumer_table)
  odbc::dbWriteTable(db_con , "costumer_table", costumer_table,overwrite  = TRUE)
  
  
  # sales tracker table
  sales_data <- data.frame(item_ID = NULL, item_name = NULL,item_category = NULL, quantity = NULL, stock_level = NULL,stock_ID = NULL, store_ID = NULL, sold_time = NULL, track_mode = NULL, comment = NULL, unit_price = NULL,store_ID = NULL,warehouse_ID = NULL)
  
  odbc::sqlCreateTable(db_con , "sales_table", sales_data)
  
  
  # create store table
  store_table <- data.frame(store_ID = "STR000",store_name = "store000",store_group = "SG0", store_adress = "t.b.d", store_capacity = "t.b.d")
  
  # odbc::sqlCreateTable(db_con , "store_table", store_table)
  odbc::dbWriteTable(db_con , "store_table", store_table,overwrite  = TRUE)
  
  
  # create warehouse table
  
  warehouse_table <- data.frame(warehouse_ID = "WRH0000",warehouse_name = "ourWareHouse", warehouse_adress = "uknown", warehouse_capacity = 100, warehouse_location = "t.b.d")
  
  # odbc::sqlCreateTable(db_con , "warehouse_table", warehouse_table)
  odbc::dbWriteTable(db_con , "warehouse_table", warehouse_table,overwrite  = TRUE)
  
  
  # # create raw materials warehouse table
  # 
  # warehouse_rm_table <- data.frame(warehouse_rw_ID = "WRH0000",warehouse_name = "ourWareHouse", warehouse_adress = "uknown", warehouse_capacity = 100, warehouse_location = "t.b.d")
  # 
  # # odbc::sqlCreateTable(db_con , "warehouse_rm_table", warehouse_rm_table)
  # odbc::dbWriteTable(db_con , "warehouse_rm_table", warehouse_rm_table,overwrite  = TRUE)
  
  
  # create order table
  orders_data <- data.frame(order_ID = "ORD1", costumer_ID = "CSTM1", article_ID = "ITEM1", quantity = 1, store_ID = "STR1",order_time = Sys.time(), comment = "", unit_price = 0,warehouse_ID = "WRH1")
  
  odbc::dbWriteTable(db_con , "orders_table", orders_data,overwrite  = TRUE)
  
  # create production table
  production_data <- data.frame(production_unit_ID = "PRDU0000", product_ID = "ITEM00001", quantity = 0, store_ID = "STR1",production_time = Sys.time(), comment = "", prod_cost = 0,warehouse_ID = "WRH1")
  
  odbc::dbWriteTable(db_con , "production_table", production_data,overwrite  = TRUE)

  return("done")
}

augment_forecast_object <- function(fcast_df = NULL, variable_name = NULL, flag = "forward"){
  fcast_df%>%dplyr::mutate(item_store = !!variable_name, forecast_type = flag)
}
#' prepare forecast for db upload
#' @description transform list of forecasts into a tabular row-wise output compatible for DB upload
#' @param forecast_results a list of forecast results
#' @param variables_to_drop variables that are to be removed such as actuals, corrected values
#' @export

prepare_forecast_for_db_upload <- function(forecast_results = NULL,variables_to_drop = NULL,as_of_date = NULL,remove_nas = TRUE,sc_meta_data = NULL){
  
  item_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="item_ID")]
  store_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="store_ID")]
  
  if(is.null(as_of_date))as_of_date <- Sys.Date()
  forecast_results <- forecast_results%>%purrr::imap(~augment_forecast_object(fcast_df = .x,variable_name = .y))%>%
    do.call(dplyr::bind_rows,.)%>%
    dplyr::select(-variables_to_drop)%>%
    dplyr::mutate(as_of_date = as_of_date,date = as.Date(date))%>%dplyr::relocate(as_of_date,.before = dplyr::everything())%>%
    tidyr::separate(item_store,c(item_ID_field,store_ID_field),sep ="\\+")
  
  if(remove_nas == TRUE)forecast_results <- forecast_results%>%tidyr::drop_na()
  return(forecast_results)
}


sql_to_csvs <- function(db_con = NULL, table_pattern = NULL, output_folder = NULL){
  available_table <- odbc::dbListTables(db_con)
  if(!is.null(table_pattern)){
    available_table <- available_table[grepl(table_pattern,available_table)]
  }
  temp_func <- function(db_con = NULL,x = NULL){
    x2 <<- x
    odbc::dbGetQuery(db_con , statement = paste0("SELECT * FROM ",x))%>%
      readr::write_csv(.,file = paste0(output_folder,"/",x,".csv"))
  }
  if(length(available_table)>0){
    output_folder%>%dir.create()
    available_table%>%purrr::map(~temp_func(db_con ,x = .x))
  }
}