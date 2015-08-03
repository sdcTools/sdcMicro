.onAttach <- function(lib, pkg) {
  msg <- "--------\n"
  msg <- paste0(msg, "This is sdcMicro v",utils::packageVersion("sdcMicro"),".\n")
  msg <- paste0(msg, "For references, please have a look at citation('sdcMicro')\n")
  msg <- paste0(msg, "Note: since version 2.6.6, the graphical user-interface is provided by package sdcMicroGUI.\n")
  msg <- paste0(msg, "Please submit suggestions and bugs at: https://github.com/alexkowa/sdcMicro/issues\n")
  msg <- paste0(msg,"--------")
  packageStartupMessage(msg)
}
