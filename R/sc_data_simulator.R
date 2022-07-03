distribute_day_to_real_time <- function(){
  
}

generate_daily_sc_activity <- function(target_day = NULL,sc_input_list = NULL, target_activity = "sales"){
  is_weekday <- target_day%>%lubridate::wday() <= 6
  if(is_weekday == TRUE){
    time_random_vector <- as.POSIXlt(paste(target_day,"09:00:00")) + rnorm(1e2, 0, 120*60)
    if(target_activity == "sales"){
      sales_arrivals <- sc_input_list$sales_avg%>%apply(MARGIN = 1, function(x)sapply(x, function(y)rpois(n = 1, y)))%>%
        data.frame()%>%mutate(product_ID =colnames(sc_input_list$sales_avg))%>%
        tidyr::pivot_longer(cols = dplyr::starts_with("STR"),names_to = "store_ID",values_to = "Sales")%>%mutate(datum = target_day)  
      return(sales_arrivals)
    }
    
    if(target_activity == "orders"){
      orders_arrivals <- sc_input_list$orders_avg%>%apply(MARGIN = 1, function(x)sapply(x, function(y)rpois(n = 1, y)))%>%
        data.frame()%>%mutate(product_ID = colnames(sc_input_list$orders_avg))%>%
        tidyr::pivot_longer(cols = dplyr::starts_with("STR"),names_to = "store_ID",values_to = "Orders")%>%mutate(datum = target_day)
      return(orders_arrivals)
    }
  }
  return(NULL)
}


sc_data_simulator_main <- function(rand_input = NULL,num_product = NULL, num_product_units = NULL,num_stores = NULL,time_period = c("2021-01-01","2022-05-30") , time_step = NULL){
  if(is.null(num_product))num_product <- max(1,rpois(1,rand_input["num_product"]))
  if(is.null(num_stores))num_stores <- max(1,rpois(1,rand_input["num_stores"]))
  
  
  if(is.null(num_product_units))num_product_units <- rpois(num_product,rand_input["num_product"])%>%sapply(function(x)max(1,x))
  names(num_product_units) <- paste0("PRD",1:num_product)
  
  sales_avg <- sapply(1:num_product,function(x)rpois(num_stores,rand_input["num_sales"])%>%sapply(function(x)max(1,x)))%>%
    data.frame()
  colnames(sales_avg) <- paste0("PRD",1:num_product)
  rownames(sales_avg) <- paste0("STR",1:num_stores)
  
  
  orders_avg <- sapply(1:num_product,function(x)rpois(num_stores,rand_input["num_sales"])%>%sapply(function(x)max(1,x)))%>%
    data.frame()
  colnames(orders_avg) <- paste0("PRD",1:num_product)
  rownames(orders_avg) <- paste0("STR",1:num_stores)
  

  
  if(is.null(num_product_units))num_product_units <- rpois(num_product,rand_input["num_product"])%>%sapply(function(x)max(1,x))
  names(num_product_units) <- paste0("PRD",1:num_product)
  
  production_capacity <- rpois(num_product_units,rand_input["production_capacity"])%>%sapply(function(x)max(1,x))
  names(production_capacity) <- paste0("CAPACITY",1:length(num_product_units))
  
  
  sc_input_list <- list(num_product = num_product, num_product_units = num_product_units,production_capacity = production_capacity,
                        sales_avg = sales_avg,orders_avg = orders_avg,num_stores = num_stores)
  
  
  time_period <- as.Date(time_period)
  time_period <- seq.Date(from = time_period[1], to = time_period[2], by = time_step)
  sales_activity <- time_period%>%sapply(generate_daily_sc_activity,sc_input_list,target_activity ="sales" )%>%
    do.call(dplyr::bind_rows,.)%>%
    distribute_day_to_real_time()
  orders_activity <- time_period%>%sapply(generate_daily_sc_activity,sc_input_list,target_activity ="orders" )%>%
    do.call(dplyr::bind_rows,.)%>%
    distribute_day_to_real_time()
  
  output_list <- list()
  output_list[["sales_activity"]] <- sales_activity
  output_list[["orders_activity"]] <- orders_activity
  return(output_list)
}
# rand_input <- c(num_product = 4,num_product_units = 3, production_capacity = 20,num_stores = 4, num_orders = 3, num_sales = 6)
# 
# time_step <- "1 day"

#
# sc_sim_results <- sc_data_simulator_main(rand_input   = rand_input,
#                                          time_period  = c("2022-01-01","2022-05-30"), 
#                                          time_step    = time_step)
#


