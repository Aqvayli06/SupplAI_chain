generate_new_order_ID <- function(db_con = NULL, store_ID = NULL){
  db_query <- paste0("SELECT order_ID FROM orders_table WHERE store_ID == '", store_ID,"' ORDER BY order_ID DESC LIMIT 1")
  last_order <- odbc::dbGetQuery(conn = db_con , statement = db_query)%>%dplyr::pull(order_ID)%>%gsub("ORD","",.)%>%as.numeric()
  new_order <- paste0("ORD",last_order + 1)
  return(new_order)
}

#' register_new_order
#' @param target_article articles for the order is placed
#' @export

register_new_order <- function(target_article = NULL){
  target_article2 <<-target_article
  order_data <- data.frame(article_ID = target_article["item_ID"],
                                   quantity = target_article["quantity"],comment = "", 
                                   unit_price = target_article["sell_price_u"])
  return(order_data)
}
#' register_new_order_main 
#' @param db_con database connection 
#' @param costumer_ID costumer ID if existing
#' @param target_articles articles for the order is placed
#' @param expected_delivery_date when does the costumer wants his order to be delivered
#' @export

register_new_order_main <- function(store_ID = NULL, costumer_ID = NULL, 
                                    target_articles = NULL, expected_delivery_date = NULL){
  db_con <- open_warehouse_db()
  new_order_ID <- generate_new_order_ID(db_con = db_con, store_ID = store_ID)
  
  order_data <- target_articles%>%apply(MARGIN = 1,register_new_order )%>%do.call(rbind,.)%>%
    dplyr::mutate(order_time = Sys.time(), store_ID = !!store_ID,costumer_ID = !!costumer_ID,order_ID = !!new_order_ID)
  odbc::dbWriteTable(db_con, "orders_table", order_data,append = TRUE)
  
  odbc::dbDisconnect(db_con)
}

#' load pending order
#' @param order_ID order identifier
#' @param costumer_ID costumer who issued the order
#' @param order_date date of order creation
#' @param update_price whether to use the old price or current one
#' @export

load_pending_order <- function(order_ID = NULL,costumer_ID = NULL, order_date = NULL, update_price = FALSE){
  
}
