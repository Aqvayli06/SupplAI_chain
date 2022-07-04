
load_calendars <- function(target_countries = NULL){
  # availables_ce
  
  calendars <- RQuantLib::calendars
  
  bizdays::load_quantlib_calendars(ql_calendars  = calendars, from = "2005-01-01", to = "2025-12-31") 
}