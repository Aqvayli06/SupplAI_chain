#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = ui <- shinymanager::secure_app(app_ui, enable_admin = TRUE, language  = "fr") , 
      server = app_server,
      options = list( launch.browser = T)
    ), 
    golem_opts = list(...)
  )
}
