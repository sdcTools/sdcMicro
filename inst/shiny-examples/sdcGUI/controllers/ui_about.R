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


output$btn_update_export_path <- renderUI({
  if (is.null(input$path_export_data)) {
    return(NULL)
  }
  if (input$path_export_data=="") {
    return(NULL)
  }
  if (!dir.exists(input$path_export_data)) {
    return(myActionButton("btn_update_export_path_xxx", "The specified directory does not exist, thus the path can't be updated", btn.style="default"))
  }
  if (file.access(input$path_export_data, mode=2)!=0) {
    return(myActionButton("btn_update_export_path_xxx", "The specified directory is not writeable, thus the path can't be updated!", btn.style="default"))
  }
  return(myActionButton("btn_update_export_path", "Update the current output path", btn.style="primary"))
})

observeEvent(input$btn_update_export_path, {
  obj$path_export <- input$path_export_data
})

output$stop_btn <- renderUI({
  btn <- bsButton("stop_sdcGUI", label="Stop the GUI", style="primary",
   onclick="setTimeout(function(){window.close();},500);")
  fluidRow(
    column(12, btn, tags$br(), tags$br(), align="center")
  )
})
observeEvent(input$stop_sdcGUI,{
  res <- reactiveValuesToList(obj)
  res <- res[c("inputdata","sdcObj")]
  stopApp(invisible(res))
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

  pp <- textInput("path_export_data", label=h5("Enter a directory where any exported files (data, script, problem instances) should be saved to"),
    placeholder=paste("e.g:", getwd()), width="50%")
  out <- list(out, fluidRow(
    column(12, h4("GUI-Settings"), align="center"),
    column(12, p("Below you can opt to change the default path, where all output from the GUI will be saved. You can change this path any time later as well."),align="center"),
    column(12, p("Currently, all output will be saved to",code(obj$path_export),"."), align="center"),
    column(12, pp, align="center"),
    column(12, uiOutput("btn_update_export_path"), align="center")
  ))

  # stop the app
  out <- list(out, uiOutput("stop_btn"))

  btn1 <- bsButton("help_about", label="", icon=icon("question"), style = "primary", size="extra-small", type="action", block = FALSE, disabled = FALSE, value = FALSE)
  out <- list(out, fluidRow(
    column(12, btn1, align="center"),
    bsTooltip("help_about", title="show some additional information", placement="bottom", trigger="hover", options = NULL)
  ))

  out
})
