#' Run the CHICO Shiny Application
#'
#' This function launches the CHICO Shiny application.
#'
#' @param ... Additional arguments passed to \code{shiny::shinyApp}
#'
#' @return A Shiny application object
#' @export
run_chico_shiny <- function(...) {

  addResourcePath("www", system.file("www", package = "chicoshiny"))
  
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...
  )
}