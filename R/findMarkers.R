#' @export
runFindMarkers <- function() {
    appDir <- system.file("shiny-app", "findMarkersApp", package = "peprDnaStability")
    if (appDir == "") {
        stop("Could not find app directory. Try re-installing `peprDnaStability`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
