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
#' @param shiny.server Setting this parameter to \code{TRUE} will return the app in the form of an
#' object rather than invoking it. This is useful for deploying \code{sdcApp} via \code{shiny-server}.
#' @export
#'
#' @examples
#' if(interactive()) {
#'   sdcApp(theme = "flatly")
#' }
sdcApp <- function(maxRequestSize=50, debug=FALSE, theme="IHSN", ..., shiny.server = FALSE) {
  if(!shiny.server)
    runApp(sdcApp(maxRequestSize, debug, theme, ..., shiny.server = TRUE))

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

  #.GlobalEnv$.startdir <- getwd()
  shinyOptions(.startdir = getwd())
  shinyOptions(.appDir = appDir)

  if (!theme %in% c("yeti","journal","flatly", "IHSN")) {
    stop("Invalid value for argument 'theme'\n")
  }

  if (theme=="yeti") {
    shinyOptions(.guitheme = "bootswatch_yeti.css")
    shinyOptions(.guijsfile = NULL)
  }

  if (theme=="journal") {
    shinyOptions(.guitheme = "bootswatch_journal.css")
    shinyOptions(.guijsfile = NULL)
  }
  if (theme=="flatly") {
    shinyOptions(.guitheme = "bootswatch_flatly.css")
    shinyOptions(.guijsfile = NULL)
  }

  if (theme=="IHSN") {
    shinyOptions(.guitheme = "ihsn-root.css")
    shinyOptions(.guijsfile = "js/ihsn-style.js")
  }

  source_from_appdir <- function(filename){
    source(file.path(appDir, filename), local = parent.frame(), chdir = TRUE)$value
  }

  shinyOptions(sdcAppInvoked = TRUE)
  source_from_appdir("global.R")
  shinyOptions(sdcAppInvoked = NULL)

  shiny::shinyApp(
    ui = source_from_appdir("ui.R"),
    server = source_from_appdir("server.R"),
    options = list(launch.browser=TRUE, ...)
  )
}
