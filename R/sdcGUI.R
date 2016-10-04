#' sdcGUI
#'
#' starts the graphical user interface developed with \emph{shiny}.
#'
#' @param maxRequestSize (numeric) number defining the maximum allowed filesize (in megabytes)
#' for uploaded files, defaults to 50MB
#' @param debug logical if \code{TRUE}, set shiny-debugging options
#' @return starts the interactive graphical user interface which may be used to perform the
#' anonymisation process.
#' @export
#'
#' @examples
#' \dontrun{
#' sdcGUI()
#' }
sdcGUI <- function(maxRequestSize=50, debug=FALSE) {
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
  if (debug) {
    options(shiny.fullstacktrace=TRUE)
    options(shiny.trace=TRUE)
  }
  shiny::runApp(appDir, display.mode="normal", launch.browser=TRUE)
}
