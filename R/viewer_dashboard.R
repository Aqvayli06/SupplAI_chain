#' run_viewer_dashboard
#'@description a flexdashboard which will be used for viewer user without a need to operate any changes
#'@param target_dashboard the dashboard for which they should access to
#'@export
run_viewer_dashboard <- function(target_dashboard = "supplai_viewer1"){
  target_dashboard <- paste0("dashboards/",target_dashboard,".Rmd")
  viewer_dashboard_file <- system.file(target_dashboard, package = "SupplAI")   
  rmarkdown::run(file = viewer_dashboard_file, shiny_args = list(port = 3838, host = '0.0.0.0'))
}