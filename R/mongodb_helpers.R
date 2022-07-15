mongodb_create_filter_query <- function(sc_meta_data = NULL, item_IDs = NULL, store_IDs = NULL){
  item_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="item_ID")]
  store_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="store_ID")]
  if(is.null(item_IDs) & is.null(store_IDs))return(NULL)
  filter_query <- list()
  if(!is.null(item_IDs)){
    if(length(item_IDs)==1){
      filter_query[[item_ID_field]] <- item_IDs
    }else{
      filter_query[[item_ID_field]] <- list('$in' = item_IDs)  
    }
  }
  
  if(!is.null(store_IDs)){
    if(length(store_IDs)==1){
      filter_query[[store_ID_field]] <- store_IDs
    }else{
      filter_query[[store_ID_field]] <- list('$in' = store_IDs)  
    }
  }
  return(filter_query)
}

#' import_data_mongo_db
#' @description import data from mongo DB databse stored on cloud based on some criteria
#' @param base_url a base url for mongoDB DATA API
#' @expo
import_data_mongo_db <- function(base_url = NULL,mongo_db_api_key = NULL,target_db = "DB_Full_byline",
                                 target_table = NULL, target_filter = NULL,req_limit = 5000) {
  
  
  
  request_body = list(
    "dataSource"= "Cluster0",
    "database"= target_db,
    "collection" = target_table,
    "limit"= req_limit)
  
  if(!is.null(target_filter)){
    request_body$filter <- target_filter
  }
  db_headers = httr::add_headers(
    'Access-Control-Request-Headers'= '*',
    'api-key'= mongo_db_api_key
  )
  
  
  # url              <- "https://data.mongodb-api.com/app/data-qgigl/endpoint/data/v1/action/find"
  url <- paste0(base_url ,"v1/action/find")
  
  api_results <- httr::POST(url = url, config = db_headers, body = request_body,encode  = "json")%>%
    httr::content(encoding = "json")
  
  
  api_results<- 
    api_results$documents%>%lapply(data.frame,check.names = FALSE)%>%
    do.call(rbind,.)%>%data.frame(check.names = FALSE)
  
  return(api_results)
}


#' upload_data_mongo_db
#' @description import data from mongo DB databse stored on cloud based on some criteria
#' @param base_url a base url for mongoDB DATA API
#' @expo
upload_data_mongo_db <- function(base_url = NULL,mongo_db_api_key = NULL,target_db = "DB_Full_byline",
                                 target_table = NULL, input_data = NULL,req_limit = 5000) {
  input_data <- input_data%>% purrr::transpose() # transform into document
  
  request_body = list(
    "dataSource" = "Cluster0",
    "database"   = target_db,
    "collection" = target_table,
    "documents"  = input_data)
  
  
  db_headers = httr::add_headers(
    'Access-Control-Request-Headers'= '*',
    'api-key'= mongo_db_api_key
  )
  
  
  # url              <- "https://data.mongodb-api.com/app/data-qgigl/endpoint/data/v1/action/find"
  url <- paste0(base_url ,"v1/action/insertMany")
  
  api_results <- httr::POST(url = url, config = db_headers, body = request_body,encode  = "json")
  
  # 
  # api_results<- 
  #   api_results$documents%>%lapply(data.frame,check.names = FALSE)%>%
  #   do.call(rbind,.)%>%data.frame(check.names = FALSE)
  # 
  return(api_results)
}