#' @export
RunApp <- function() {
  # appDir <- system.file("app", package = "TreeData")
  appDir <- "inst/app"
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
