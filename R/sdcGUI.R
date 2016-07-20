#' sdcGUI
#'
#' starts the graphical user interface developed with \emph{shiny}.
#'
#' @param maxRequestSize (numeric) number defining the maximum allowed filesize (in megabytes)
#' for uploaded files, defaults to 50MB
#' @return starts the interactive graphical user interface which may be used to perform the
#' anonymisation process.
#' @export
#'
#' @examples
#' \dontrun{
#' sdcGUI()
#' }
sdcGUI <- function(maxRequestSize=50) {
  if (!is.numeric(maxRequestSize)) {
    stop("argument 'maxRequestSize' must be numeric!\n")
  }
  if (maxRequestSize < 1) {
    maxRequestSize <- 10
  }
  appDir <- system.file("shiny-examples", "sdcGUI", package="sdcMicro")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sdcMicro`.", call.=FALSE)
  }
  options(shiny.maxRequestSize=ceiling(maxRequestSize)*1024^2)
  shiny::runApp(appDir, display.mode="normal", launch.browser=TRUE)
}
