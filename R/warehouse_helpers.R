#' open_warehouse_d
#' @description t.b.d
#' @export
open_warehouse_db <- function(db_name = "./inst/db_warehouse_mgmt.sqlite"){
  odbc::dbConnect(RSQLite::SQLite(), dbname = db_name)
}

#' compute available quantity
#' @description  t.b.d
#' @param selected_articles articles already selected
#' @param intial_quantity additional quantity we want to add
#' @export
compute_available_quantity <- function(target_article = NULL,selected_articles = NULL,intial_quantity = NULL ){
  if(is.null(selected_articles))return(intial_quantity)
  selected_quantity <-selected_articles%>%
    dplyr::filter(item_name == !!target_article)%>%
    dplyr::pull(quantity)
  if(length(selected_quantity) == 0)return(intial_quantity)
  remaining_quantity <- intial_quantity -  selected_quantity
  return(remaining_quantity)
}


#' update_sold_article_in_warehouse
#' @description update a given article in the warehouse
#' @param target_article single article
#' @param db_con database connection
update_sold_article_in_warehouse <- function(target_article = NULL,db_con = NULL){
  query_req <- paste0("SELECT * FROM product_table WHERE item_name = '",target_article["item_name"],"'")
  res <- odbc::dbSendQuery(db_con, query_req)
  article_warehouse <- odbc::dbFetch(res)
  odbc::dbClearResult(res)
  article_warehouse$quantity_disponible <- as.numeric(article_warehouse$quantity_disponible) - as.numeric(target_article["quantity"])
  article_warehouse
  query_req <- gsub("SELECT","DELETE",query_req)
  query_req <- gsub("\\*","",query_req)
  res <- odbc::dbSendQuery(db_con, query_req)
  odbc::dbClearResult(res)
  odbc::dbWriteTable(db_con, "product_table", article_warehouse,append = TRUE)
  
  sales_data <- data.frame(item_name = target_article["item_name"], quantity = target_article["quantity"], stock_level = article_warehouse$quantity_disponible, sold_time = Sys.time(), track_mode = "sales", comment = NA, unit_price = target_article["sell_price_u"],store_ID = NA,warehouse_ID = NA)
  odbc::dbWriteTable(db_con, "sales_table", sales_data,append = TRUE)
}


#' update warehouse 
#' @description t.b.d
#' @param update_mode sales or replenishment or return
#' @param targe_articles articles for which to update the warehouse

update_warehouse <- function(target_articles = NULL, update_mode = NULL){
  db_con <- open_warehouse_db()
  if(update_mode =="sales"){
    target_articles <- target_articles%>%
      head(-5)%>% # remove unnecessary rows
      tidyr::drop_na(item_name)  # remove NA rows
    target_articles%>%apply(1,update_sold_article_in_warehouse,db_con)
  }
  odbc::dbDisconnect(db_con)
}

#' get augmented sales data
#' @param selected_articles items selected to sell
#' @export
get_augmented_sales_data <- function(selected_articles = NULL,sell_discount = NULL){
  sell_discount <- as.numeric(sell_discount) 
  table_finale <- selected_articles%>%dplyr::as.tbl()%>%
     dplyr::mutate(somme = as.numeric(sell_price_u) * quantity)
   
   total  <- data.frame(item_name = "Total Sum",sell_price_u = NA, quantity = sum(table_finale$quantity), somme = sum(table_finale$somme))%>%dplyr::as.tbl()
   na_df <- discount <- data.frame(item_name = NA,sell_price_u = NA, quantity = NA, somme = NA)
   
   discount <- data.frame(item_name = "Discount",sell_price_u = NA, quantity = NA, somme = total$somme * (sell_discount/100))%>%dplyr::as.tbl()
   
   montant <- data.frame(item_name = "To Pay",sell_price_u = NA, quantity = NA, somme = total$somme - sell_discount)%>%dplyr::as.tbl()
   
   table_finale <- table_finale%>%dplyr::bind_rows(na_df)%>%
     dplyr::bind_rows(total)%>%dplyr::bind_rows(na_df)%>%
     dplyr::bind_rows(discount)%>%dplyr::bind_rows(na_df)%>%dplyr::bind_rows(montant)
}

#' upload a data table to database
#' @description t.b.d
#' @param input_data a dataframe to upload into DB
#' @param target_table target DB table : sales_table, store_table, product_table, warehouse_table, orders_table
#' @param meta_data additional information about the data table
#' @param meta_data_file a json where the overall metadata is stored 

upload_to_database <- function(input_data = NULL, target_table = NULL,upload_mode = "overwrite" ,meta_data = NULL, meta_data_file = "./inst/sc_project_metadata.json"){
  db_con <- open_warehouse_db()
  input_data <- input_data%>%janitor::clean_names()
  date_vars <- NULL
  
  # if("date" %in% colnames(input_data))date_vars <- c("date" = "DATE")
  db_overwite <- db_append <- FALSE
  if(upload_mode == "overwrite")db_overwite <- TRUE
  if(upload_mode == "append")db_append <- TRUE
  odbc::sqlCreateTable(db_con , target_table, input_data)
  odbc::dbWriteTable(db_con,  target_table, input_data, overwrite = db_overwite, append = db_append,field.types = date_vars)
  if(file.exists(meta_data_file)){
    sc_metadata <- load_db_metadata(sc_project_metadata_file = meta_data_file)  
  }else{
    sc_metadata <- get_db_mapping_info(db_con = db_con) 
  }
  sc_metadata$db_tables <- c(sc_metadata$db_tables,target_table)%>%unique()
  sc_metadata$db_available_attributes[[target_table]] <- colnames(input_data)
  if(!is.null(meta_data))sc_metadata$db_tables_names <- c(sc_metadata$db_tables_names ,meta_data[["table_name"]])%>%unique()
  if(!is.null(meta_data))sc_metadata$table_attributes_names[[target_table]] <- meta_data[["attributes_names"]]
  if(!is.null(meta_data))sc_metadata$date_format[[target_table]] <- meta_data[["date_format"]]
  
  sc_metadata%>%jsonlite::toJSON()%>%jsonlite::write_json(path = meta_data_file)
  odbc::dbDisconnect(db_con)
  return(sc_metadata)
}


upload_to_database_main <- function(){
  
  meta_data_file <- "./inst/sc_project_metadata.json"
  sales_data <- readr::read_csv("./real_data/sales_retail_medium_daily.csv")
  
  sales_attributes_names <- c("sale_ID","date","store_ID","item_ID", "quantity", "on_promotion")
  
  sales_meta   <- list(table_name = "Sales", attributes_names = sales_attributes_names, date_format = "%Y-%m-%d")
  
  sc_metadata  <- upload_to_database(input_data = sales_data, target_table = "sales_table", 
                                    meta_data_file = meta_data_file, meta_data = sales_meta)
  
  store_data <- readr::read_csv("./real_data/stores.csv")
  
  
  store_attributes_names <- c("store_ID","store_adress","state","store_type","store_group")
  
  store_meta <- list(table_name = "Store", attributes_names = store_attributes_names)
  
  sc_metadata <- upload_to_database(input_data = store_data, target_table = "store_table", 
                                    meta_data_file = meta_data_file, meta_data = store_meta)
  
  
  store_attributes_names <- c("store_ID","store_adress","state","store_type","store_group")
  
  store_meta <- list(table_name = "Store", attributes_names = store_attributes_names)
  
  sc_metadata <- upload_to_database(input_data = store_data, target_table = "store_table", 
                                    meta_data_file = meta_data_file, meta_data = store_meta)
  return(sc_metadata)
}
