# output$pkg_status <- renderUI({
#   s <- obj$pkg_status
#   if (s$isOk==TRUE) {
#     return(fluidRow(
#       column(12, p("sdcMicro is running in version",code(s$cranV),"and is up-to date!"),align="center")
#     ))
#   } else {
#     return(fluidRow(
#       column(12, p("Your version of sdcMicro is non-running the latest CRAN-version which is",code(paste0(s$cranV,".")),"Please update the package!"),align="center")
#     ))
#   }
# })
observeEvent(input$help_about, {
  showModal(modalDialog(
    title = "About the GUI",
    "We can show some important information here!",
    easyClose = TRUE
  ))
})


output$ui_about <- renderUI({
  out <- fluidRow(
    column(12, h4("About the Interface", align="center")),
    column(12, p("Some information about",code("sdcMicro"),"and how to use this graphical interface."), align="center"))
    #out <- list(out, uiOutput("pkg_status"))
    out <- list(out, fluidRow(
      column(12, h4("Help"), align="center"),
      column(12, p("Link to GUI-Tutorial"), align="center"),
      column(12, p("Link to GUI-Tutorial"), align="center"),
      column(12, p("If you already have an sdcProblem that was exported from the GUI, you can upload it in Tab",code("Reproducibility"),"."), align="center")
  ))

  btn1 <- bsButton("help_about", label="", icon=icon("question"), style = "primary", size="extra-small", type="action", block = FALSE, disabled = FALSE, value = FALSE)
  out <- list(out, fluidRow(
    column(12, btn1, align="center"),
    bsTooltip("help_about", title="show some additional information", placement="bottom", trigger="hover", options = NULL)
  ))
  out
})
