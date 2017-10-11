#' sdcApp
#'
#' starts the graphical user interface developed with \emph{shiny}.
#'
#' @param maxRequestSize (numeric) number defining the maximum allowed filesize (in megabytes)
#' for uploaded files, defaults to 50MB
#' @param debug logical if \code{TRUE}, set shiny-debugging options
#' @return starts the interactive graphical user interface which may be used to perform the
#' anonymisation process.
#' @param theme select stylesheet for the interface. Supported choices are
#' \itemize{
#' \item 'yeti'
#' \item 'flatly'
#' \item 'journal'
#' \item 'IHSN'
#' }
#' @param ... arguments (e.g \code{host}) that are passed through \code{\link[shiny]{runApp}} when
#' starting the shiny application
#' @export
#'
#' @examples
#' \dontrun{
#' sdcApp(theme="flatly")
#' }
sdcApp <- function(maxRequestSize=50, debug=FALSE, theme="IHSN", ...) {
  .startdir <- .guitheme <- .guijsfile <- NULL
  if (!is.numeric(maxRequestSize)) {
    stop("argument 'maxRequestSize' must be numeric!\n")
  }
  if (maxRequestSize < 1) {
    maxRequestSize <- 10
  }
  appDir <- system.file("shiny", "sdcApp", package="sdcMicro")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sdcMicro`.", call.=FALSE)
  }
  options(shiny.maxRequestSize=ceiling(maxRequestSize)*1024^2)
  options(shiny.fullstacktrace=debug)
  options(shiny.trace=debug)

  .GlobalEnv$.startdir <- getwd()
  on.exit(rm(.startdir, envir=.GlobalEnv))

  if (!theme %in% c("yeti","journal","flatly", "IHSN")) {
    stop("Invalid value for argument 'theme'\n")
  }

  if (theme=="yeti") {
    .GlobalEnv$.guitheme <- "bootswatch_yeti.css"
    .GlobalEnv$.guijsfile <- NULL
  }

  if (theme=="journal") {
    .GlobalEnv$.guitheme <- "bootswatch_journal.css"
    .GlobalEnv$.guijsfile <- NULL
  }
  if (theme=="flatly") {
    .GlobalEnv$.guitheme <- "bootswatch_flatly.css"
    .GlobalEnv$.guijsfile <- NULL
  }

  if (theme=="IHSN") {
    .GlobalEnv$.guitheme <- "ihsn-root.css"
    .GlobalEnv$.guijsfile <- "js/ihsn-style.js"
  }

  on.exit(rm(.guitheme, envir=.GlobalEnv))

  shiny::runApp(appDir, display.mode="normal", launch.browser=TRUE, ...)
}
