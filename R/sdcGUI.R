#' sdcGUI
#'
#' @return starts the interactive graphical user interface which may be used to perform the
#' anonymisation process.
#' @export
#'
#' @examples
#' \dontrun{
#' sdcGUI()
#' }
sdcGUI <- function() {
  appDir <- system.file("shiny-examples", "sdcGUI", package="sdcMicro")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sdcMicro`.", call.=FALSE)
  }
  shiny::runApp(appDir, display.mode="normal", launch.browser=TRUE)
}
