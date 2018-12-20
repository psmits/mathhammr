#' Run example shiny app
#'
#' from https://deanattali.com/2015/04/21/r-package-shiny-app/
#'
#' @export
run_example <- function() {
  appDir <- system.file("shiny-examples", "app", package = "mathhammr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
