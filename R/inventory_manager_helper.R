#' load_sales_forecasts
#' @param db_con a database connection
#' @param item_IDs articles to load
#' @param store_IDs store IDs
#' @export

load_sales_forecasts <- function(db_con = NULL,target_attribute = "*", item_IDs = NULL, store_IDs = NULL,as_of_date = NULL,sc_meta_data = NULL,date_freq = NULL,fcast_horizon = 14){
  if(is.null(as_of_date))as_of_date <- Sys.Date()
  fcast_horizon <- as.numeric(fcast_horizon)
  query_statement <- paste0("SELECT ",target_attribute," FROM sales_forecast_table WHERE as_of_date ='",as.numeric(as_of_date),"'")
  sales_forecasts <- odbc::dbGetQuery(conn = db_con,statement = query_statement)
  if(is.null(item_IDs) | is.null(store_IDs))return(sales_forecasts)
  
  item_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="item_ID")]
  store_ID_field <- sc_meta_data$db_available_attributes$sales_table[which(sc_meta_data$table_attributes_names[["sales_table"]] =="store_ID")]
  
  sales_forecasts <- sales_forecasts%>%
    dplyr::filter(.data[[item_ID_field]] %in% !! item_IDs &
                    .data[[store_ID_field]] %in% !! store_IDs)
  
  date_vect   <- sales_forecasts%>%dplyr::pull(date)%>%lubridate::as_date(., origin = lubridate::origin) # convert date into the right format
  sales_forecasts  <- sales_forecasts%>%dplyr::mutate(date = date_vect)
  date_vect   <- sales_forecasts%>%dplyr::pull(as_of_date)%>%lubridate::as_date(., origin = lubridate::origin) # convert date into the right format
  sales_forecasts  <- sales_forecasts%>%dplyr::mutate(as_of_date = date_vect)%>%
    head(fcast_horizon)
  return(sales_forecasts)
}


generate_sales_forecast_chart <- function (fcast_df = NULL,
                                           plot_settings = NULL, target_variable = "Sales Forecast"){
  if (is.null(plot_settings)) {
    forecast_col <- "#1B9E77"
    ConfInt_col <- "#666666"
  } else {
    forecast_col <- plot_settings$colors_inu[1]
    ConfInt_col <- plot_settings$colors_inu[1]
  }
  fcast_plot <- plotly::plot_ly(data = fcast_df)%>% 
    plotly::add_lines(x = ~date, y = ~forecast, color = I(forecast_col), 
                      name = "prediction") %>% plotly::add_ribbons(x = ~date, 
                                                                   ymin = fcast_df$lower, ymax = fcast_df$upper, color = I(ConfInt_col), 
                                                                   name = "95% confidence")
  fcast_plot <- fcast_plot %>% plotly::layout(plot_bgcolor = NULL, 
                                              paper_bgcolor = NULL, yaxis = list(title = target_variable), 
                                              legend = list(orientation = "h", x = 0.35, y = 100)) %>% 
    plotly::config(displaylogo = F)
  return(fcast_plot)
}

calculate_projected_inventory <- function(demand_forecast = NULL,safety_stock = 2, supply_freq = 3){

  date_vect     <- demand_forecast%>%dplyr::pull(date)
  demand_fcast  <- -1* demand_forecast%>%dplyr::pull(forecast)%>%ceiling()
  
  safety_stock_min <- - 1 * (1:(length(demand_fcast) - safety_stock))%>%sapply(function(x)sum(demand_fcast[x:(x+safety_stock-1)]))
  safety_stock_max <- - 1 * (1:(length(demand_fcast) - safety_stock))%>%sapply(function(x)sum(demand_fcast[x:(x+safety_stock+supply_freq-1)]))
  
  stock_level <- 0
  replenishment_start <- 2
  
  drp_plan_indx <- seq(replenishment_start,length(demand_fcast),supply_freq)
  projected_inventory <- max(stock_level,safety_stock_min)
  drp_amount <- 0 
  for(i in 2:(length(safety_stock_max)-1)){
    drp_amount[i] <- 0
    if(i %in% drp_plan_indx){
      drp_amount[i] <- safety_stock_max[i] - projected_inventory[i-1]  - demand_fcast[i]
    }
    projected_inventory[i]  <-  projected_inventory[i-1] + drp_amount[i] +demand_fcast[i]
  }
  fcast_horizon <- length(safety_stock_max) - 1
  inventory_output <- data.frame(date = head(date_vect,fcast_horizon), demand_fcast = head(-1 * demand_fcast,fcast_horizon),
                                 projected_inventory = head(projected_inventory,fcast_horizon),
                                 safety_stock_min  = head(safety_stock_min,fcast_horizon),
                                 safety_stock_max  = head(safety_stock_max,fcast_horizon),
                                 drp_amount = head(drp_amount,fcast_horizon))
  warning("calculate_projected_inventory : fix the NA and remove the command line below")
  inventory_output<- inventory_output%>%tidyr::drop_na()
  return(inventory_output)
}

#' calculate_projected_inventory
#' @param stock_level a stock level at a given time
#' @param demand_forecast demand forecast to use to calculate projected inventory
#' @param frozen_period  a period where no replenishment is possible
#' @param safety_stock a period of time for which current stock should serve before going to shortage
#' @param supply_freq frequency of Supply
#' @export

calculate_projected_inventory_main <- function( demand_forecast = NULL, stock_level = NULL, frozen_period = NULL,upcoming_orders = NULL,safety_stock = 2, supply_freq = 3, projection_horizon = 12){
  supply_freq <- as.numeric(supply_freq)
  safety_stock <- as.numeric(safety_stock)
  projection_horizon <- as.numeric(projection_horizon)
  # demand_forecast <- demand_forecast%>%dplyr::filter(family== "BEVERAGES" & store_nbr =="2")
  demand_forecast <- demand_forecast
  projected_inventory_dt <- calculate_projected_inventory(demand_forecast,safety_stock = safety_stock, supply_freq = supply_freq)%>%
    head(projection_horizon)  
  return(projected_inventory_dt)
}

#' generate_projected_inventory_chart
#' @param projected_inventory_dt a dataframe containing prjected inventory information
#' @export

generate_projected_inventory_chart <- function(projected_inventory_dt = NULL){
  project_invent_chart <- projected_inventory_dt%>%
    plotly::plot_ly(data = .)%>% 
    plotly::add_trace(x = ~date, y = ~projected_inventory, color = I("#E6AB02"),mode ="lines+markers", 
                      name = "Projected Inventory") %>% 
    plotly::add_ribbons(x = ~date, 
                        ymin = ~safety_stock_min, ymax = ~safety_stock_max, color = I("#3c8dbc"), 
                        name = "Min-Max Stock")
  project_invent_chart <- project_invent_chart %>% plotly::layout(plot_bgcolor = NULL, 
                                              paper_bgcolor = NULL, yaxis = list(title = "units"), 
                                              legend = list(orientation = "h", x = 0.35, y = 100)) %>% 
    plotly::config(displaylogo = F)
  return(project_invent_chart)
}
