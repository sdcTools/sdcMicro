# output$pkg_status <- renderUI({
#   s <- obj$pkg_status
#   if (s$isOk==TRUE) {
#    return(fluidRow(
#      column(12, p("sdcMicro is running in version",code(s$cranV),"and is up-to date!"),align="center")
#    ))
#   } else {
#    return(fluidRow(
#      column(12, p("Your version of sdcMicro is non-running the latest CRAN-version which is",code(paste0(s$cranV,".")),"Please update the package!"),align="center")
#    ))
#   }
# })

output$btn_update_export_path <- renderUI({
  if (is.null(input$path_export_data)) {
    return(NULL)
  }
  if (input$path_export_data=="") {
    return(NULL)
  }
  if (!dir.exists(input$path_export_data)) {
    return(myActionButton("btn_update_export_path_xxx", "The specified directory does not exist, thus the path can't be updated", btn.style="danger"))
  }
  if (file.access(input$path_export_data, mode=2)!=0) {
    return(myActionButton("btn_update_export_path_xxx", "The specified directory is not writeable, thus the path can't be updated!", btn.style="danger"))
  }
  return(myActionButton("btn_update_export_path", "Update the current output path", btn.style="primary"))
})

observeEvent(input$btn_update_export_path, {
  obj$path_export <- input$path_export_data
})

output$stop_btn <- renderUI({
  btn <- bsButton("stop_sdcGUI", label="Stop the GUI", style="primary", size="extra-small",
   onclick="setTimeout(function(){window.close();},500);")
  btn
})
observeEvent(input$stop_sdcGUI,{
  res <- reactiveValuesToList(obj)
  res <- res[c("inputdata","sdcObj")]
  stopApp(invisible(res))
})

output$ui_about <- renderUI({
  fluidRow(
    column(12, h4("About the Interface", align="center")),
    column(12, p("Some information about",code("sdcMicro"),"and how to use this graphical interface."), align="center"),
    column(12, h4("Help"), align="center"),
    column(12, p("Link to GUI-Tutorial"), align="center"),
    column(12, p("Link to GUI-Tutorial"), align="center"),
    column(12, p("If you already have an sdcProblem that was exported from the GUI, you can upload it in Tab",code("Reproducibility"),"."), align="center")
  )
  out <- list(out, fluidRow(
    column(12, p("Detailed information on how to use this graphical user-interface (UI) can be found in a vignette that is included in",code("sdcMicro"),".
    You can read the vignette by typing",code('vignette("sdcGUI", package="sdcMicro")'),"into your",code("R"),"prompt."), align="center")
  ))

  if (is.null(inputdata())) {
    btn <- bsButton(paste0("btn_a_micro_ui_about"), label=("this button"), style="primary", size="extra-small")
    btn_pi <- bsButton(paste0("nodata_script_about"), label=("here"), style="primary", size="extra-small")

    txt_start <- paste0("To get started, you need to upload some micro data to the GUI. You can do so by clicking on ",btn,". ")
    txt_start <- paste0(txt_start, "Alternatively, you can also upload a previously saved problem instance by clicking ",btn_pi,".")
    out <- list(out, fluidRow(
      column(12, h4("Getting started"), align="center"),
      column(12, p(HTML(txt_start)), align="center")
    ))
  }

  pp <- textInput("path_export_data", label=h5("Enter a directory where any exported files (data, script, problem instances) should be saved to"),
    placeholder=paste("e.g:", obj$path_export), width="50%")
  stop_btn <- bsButton("stop_sdcGUI", label="Stop the GUI", style="primary", size="extra-small", onclick="setTimeout(function(){window.close();},500);")
  out <- list(out, fluidRow(
    column(12, h4("Settings"), align="center"),
    column(12, h5("Set storage path"), align="center"),
    column(12, p("Below you can change the default path, where all output from the GUI will be saved. You can change this path any time
      later as well by returing to this tab."),align="center"),
    column(12, p("Currently, all output, such as anonymized data, scripts and reports, will be saved to",code(obj$path_export),"."), align="center"),
    column(12, pp, align="center"),
    column(12, uiOutput("btn_update_export_path"), align="center"),
    column(12, h5("Stop the interface"), align="center"),
    column(12, p("By clicking on", stop_btn,", you can stop the the graphical user interface at any time during the anonymization process.
      If you have started the interface as",code('x <- sdcGUI()'),",",code("x"),
      "will contain the micro data and the sdc problem at the state just before stopping the GUI."), align="center"),
    column(12, h5("Restart the interface"), align="center"),
    column(12, p("Should you accidentally close this browser window, you can open your current problem instance by entering the local IP address
      specified in the",code("R"),"console in which you started the GUI. The address starts with", code("http://127.0.0.1:"), ". You
      can also use the refresh button of your browser in case the GUI freezes."), align="center")
  ))

  out <- list(out, fluidRow(
    column(12, h4("Contact and Feedback"), align="center"),
    column(12, p("In case you have any suggestions or bug reports, please file an issue at the",
      tags$a("Issue tracker", href="https://www.github.com/alexkowa/sdcMicro/issues", target="_blank"),"in our",
      tags$a("github repo", href="https://www.github.com/alexkowa/sdcMicro", target="_blank"),"."), align="center")
  ))
  out
})
