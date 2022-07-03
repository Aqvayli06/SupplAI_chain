#' display_dt_table_DT 
#' @param data_table a tabular data input
#' @param grad_col_var variables to 
#' @param col_to_hide columns to hide
#' @param new_col_names new column names

display_dt_table_DT <- function(data_table = NULL, grad_col_var = NULL,new_col_names = NULL){
  DT_data_table <- data_table%>%
    DT::datatable(options = list(pageLength = 15, autoWidth = TRUE), rownames= FALSE)
  var_colors <- names(grad_col_var)
  names(var_colors) <- grad_col_var
  for(iv in grad_col_var){
    col_range <- c(0,range(data_table%>%dplyr::pull(!!iv))[2])
    DT_data_table <- DT_data_table%>%
      DT::formatStyle(iv,
                  background = DT::styleColorBar(col_range, var_colors[iv]),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') 
  }
  return(DT_data_table)
}

grad_col_var <- c("demand_fcast","drp_amount")
names(grad_col_var) <- c("lightblue","orange")
