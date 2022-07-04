#' load_db_metadata
#' @description load metadata of the databse : port, path, tables attributes , ...
#' @param sc_project_metadata supply chain project metadata 
#' @export

load_db_metadata <- function(sc_project_metadata_file = "./inst/sc_project_metadata.json"){
   if(!file.exists(sc_project_metadata_file)){
     dir.create("inst")
     file.copy(from = system.file("sc_project_metadata.json", package = "SupplAI"),to = sc_project_metadata_file,recursive = TRUE,overwrite = TRUE)    
   }
  
   sc_project_metadata <- jsonlite::fromJSON(sc_project_metadata_file)%>%
     jsonlite::parse_json(simplifyVector  = TRUE)
   return(sc_project_metadata)
}

#' get database info
#' @param db_con database connection
#' @export

get_db_mapping_info <- function(db_con = NULL){
  db_infos <- odbc::dbGetInfo(db_con)
  db_infos$db_type <- db_infos$dbname%>%strsplit(split = "\\.")%>%unlist()%>%tail(1)
  db_infos$db_tables  <- db_con%>%odbc::dbListTables()
  names(db_infos$db_tables) <- db_infos$db_tables
  db_infos$db_available_attributes <- db_infos$db_tables%>%purrr::map(~odbc::dbListFields(db_con,.x))
  
  # db_infos$db_tables <- mapping_table_names(sc_tables = db_infos$db_tables) 
  
  warning("get_db_mapping_info : the 2 lines below to be removed")
  db_infos$db_tables <- db_infos$db_tables[!grepl("pwd_mngt|logs|credentials",db_infos$db_tables)]
  db_infos$db_tables_names <- db_infos$db_tables%>%
    gsub("_"," ",.)%>%gsub("table","",.)%>%capwords()
  # db_infos%>%jsonlite::toJSON()%>%jsonlite::write_json(path = "./inst/sc_project_metadata.json")
  
  return(db_infos)
}


#' load data from database
#' @param db_con database connection
#' @param target_table table to access from database
#' @param time_period a start data and an end date
load_data_from_database <- function(target_table = NULL,db_con = NULL, time_period = NULL){
  query_statement <- paste0("SELECT * FROM ",target_table)
  odbc::dbGetQuery(conn = db_con, statement = query_statement )
}

#' load data from database main
#' @param db_con database connection
#' @param target_tables tables to access from database
#' @param time_period a start data and an end date
#' @export
load_data_from_database_main <- function(db_con = NULL, target_tables = NULL,time_period = NULL ){
  names(target_tables) <- target_tables
  if(is.null(db_con))db_con <- open_warehouse_db()
  target_tables%>%load_data_from_database(target_table = .x , db_con = db_con, time_period = time_period)
}