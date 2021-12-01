#' @export
RunApp <- function() {
  # appDir <- system.file("app", package = "TreeData")
  appDir <- "inst/app"
  if (appDir == "") {
    stop("Could not find the Shiny app. Try re-installing the `TreeData` package.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
