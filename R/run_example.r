#' Run mathammr examples
#'
#' Launch a \code{mathammr} example Shiny app.
#'
#' Run without any arguments to see a list of available example apps.
#'
#' https://deanattali.com/2015/04/21/r-package-shiny-app/
#'
#' @param example The app to launch
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   # List all available example apps
#'   run_example()
#'
#'   run_example("dice_roller")
#' }
#' @export
run_example <- function(example) {

  valid_examples <-
    paste0('Valid examples are: "', 
           paste(list.files(system.file("examples", package = "mathhammr")), 
                 collapse = '", "'),
           '"')

  if (missing(example) || !nzchar(example)) {
    message('Please run `run_example()` with a valid example app as an argument.\n',
            valid_examples)
    return(invisible(NULL))
  }

  app_dir <- system.file("examples", example, package = "mathammer")
  if (app_dir == "") {
    errMsg(sprintf("could not find example app `%s`\n%s", example, valid_examples))
  }

  shiny::runApp(appDir, display.mode = "normal")
}
