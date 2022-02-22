.onAttach <- function(lib, pkg) {
  msg <- "--------\n"
  msg <- paste0(msg, "This is sdcMicro v",utils::packageVersion("sdcMicro"),".\n")
  msg <- paste0(msg, "For references, please have a look at citation('sdcMicro')\n")
  msg <- paste0(msg, "Note: since version 5.0.0, the graphical user-interface is a shiny-app that can be started with sdcApp().\n")
  msg <- paste0(msg, "Please submit suggestions and bugs at: https://github.com/sdcTools/sdcMicro/issues\n")
  msg <- paste0(msg,"--------")
  packageStartupMessage(msg)
}

.onAttach <- function(lib, pkg) {
  Sys.setenv("sdcMicro_maxsize_undo" = 1e5)
}
