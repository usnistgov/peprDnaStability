#' @export
runFindMarkers <- function() {
    appDir <- system.file("shiny-app", "findMarkersApp", package = "peprrDnaStability")
    if (appDir == "") {
        stop("Could not find app directory. Try re-installing `peprrDnaStability`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
