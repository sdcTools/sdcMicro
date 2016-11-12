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
    column(12, h4("sdcGUI", align="center")),
    column(12, p("Some information about",code("sdcMicro"),"and how to use this graphical interface."), align="center")
  )
  #out <- list(out, uiOutput("pkg_status"))
  out <- list(out, fluidRow(
    column(12, h4("Help"), align="center"),
    column(12, p("Detailed information on how to use this graphical user-interface (UI) can be found in a vignette that is included in",code("sdcMicro"),"."), align="center"),
    column(12, p("You can read the vignette by typing",code('vignette("sdcGUI", package="sdcMicro")'),"into your",code("R"),"prompt."), align="center")
  ))

  if (is.null(inputdata())) {
    btn <- bsButton(paste0("btn_a_micro_ui_about"), label=("this button"), style="primary", size="extra-small")
    btn_pi <- bsButton(paste0("nodata_script_about"), label=("here"), style="primary", size="extra-small")
    out <- list(out, fluidRow(
      column(12, h4("Getting started"), align="center"),
      column(12, h5("Upload microdata"), align="center"),
      column(12, p("To get started, you need to upload some micro data to the GUI. You can do so by clicking on",btn,"."), align="center"),
      column(12, h5("Upload a problem instance"), align="center"),
      column(12, p("Alternatively, you can also upload a previously saved problem instance by clicking",btn_pi,"."), align="center")
    ))

    #if (is.null(sdcObj())) {
    #  column(12, p("If you already have an sdcProblem that was exported from the GUI, you can upload it in Tab",code("Reproducibility"),"."), align="center")
    #}
  }



  pp <- textInput("path_export_data", label=h5("Enter a directory where any exported files (data, script, problem instances) should be saved to"),
    placeholder=paste("e.g:", obj$path_export), width="50%")
  out <- list(out, fluidRow(
    column(12, h4("GUI-Settings"), align="center"),
    column(12, p("Below you can opt to change the default path, where all output from the GUI will be saved. You can change this path any time later as well."),align="center"),
    column(12, p("Currently, all output will be saved to",code(obj$path_export),"."), align="center"),
    column(12, pp, align="center"),
    column(12, uiOutput("btn_update_export_path"), align="center")
  ))

  # stop the app
  out <- list(out, uiOutput("stop_btn"))
  out
})
