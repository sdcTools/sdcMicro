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
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' sdcApp(theme="flatly")
#' }
sdcApp <- function(maxRequestSize=50, debug=FALSE, theme="yeti") {
  .startdir <- .guitheme <- NULL
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

  if (!theme %in% c("yeti","journal","flatly")) {
    stop("Invalid value for argument 'theme'\n")
  }

  if (theme=="yeti") {
    .GlobalEnv$.guitheme <- "bootswatch_yeti.css"
  }

  if (theme=="journal") {
    .GlobalEnv$.guitheme <- "bootswatch_journal.css"
  }
  if (theme=="flatly") {
    .GlobalEnv$.guitheme <- "bootswatch_flatly.css"
  }
  on.exit(rm(.guitheme, envir=.GlobalEnv))

  shiny::runApp(appDir, display.mode="normal", launch.browser=TRUE)
}
