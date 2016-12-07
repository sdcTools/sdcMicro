output$credits <- renderUI({
  out <- fluidRow(
    column(12, h4("Implementation"), align="center"),
    column(12, p("This app was developed by", a(tags$strong("data-analysis OG"), href="http://www.data-analysis.at", target="_blank")), align="center"),
    column(12, tags$img(src="imgs/logo_da.jpg"), tags$br(), align="center")
  )
  out <- list(out, fluidRow(
    column(12, h4("Funding"), align="center"),
    column(12, p("The work was funded by the", a(tags$strong("World Bank Group"), href="http://www.worldbank.org/", target="_blank")), align="center"),
    column(12, tags$img(src="imgs/logo_worldbank.png"), tags$br(), align="center"),
    column(12, p("and the Department for International Development",
      a(tags$strong("DfID"), href="https://www.gov.uk/government/organisations/department-for-international-development", target="_blank")), align="center"),
    column(12, tags$img(src="imgs/logo_ukaid.png", width = 70), align="center")
  ))
  out <- list(out, fluidRow(
    column(12, h4("Special Thanks"), align="center"),
    column(12, p("We also want to thank",tags$strong("Olivier Dupriez"),"and",tags$strong("Matthew Welch"),"for the possibility to
      create this GUI as well as many constructive suggestions and improvements. We also want to thank",tags$strong("Thijs Benschop"),
      "and", tags$strong("Cathrine Machingauta"), "for testing, reporting issues and many contributions that improved the quality of
      the final product."), align="center")
  ))
  out
})

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
  btn <- bsButton("stop_sdcApp", label="Stop the GUI", style="primary", size="extra-small",
   onclick="setTimeout(function(){window.close();},500);")
  btn
})
observeEvent(input$stop_sdcApp,{
  res <- reactiveValuesToList(obj)
  res <- res[c("inputdata","sdcObj")]
  stopApp(invisible(res))
})

output$ui_about <- renderUI({
  btn_credits <- bsButton("btn_credits", "here", style="primary", size="extra-small")
  out <- fluidRow(
    column(12, h4(strong("sdcApp"), align="center")),
    column(12, p("This graphical user interface of",code("sdcMicro")," allows you to anonymize microdata even if you are not an
      expert in the",code("R"),"programming language. Detailed information on how to use this graphical user-interface (GUI) can be found in a tutorial (a so-called vignette) that is included in the",code("sdcMicro"),"package.
    You can read the vignette by typing",code('vignette("sdcApp", package="sdcMicro")'),"into your",code("R"),"prompt."), align="center"),
    column(12, p("For information on the support and development of the graphical user interface, please click", btn_credits,"."), align="center"),
    bsModal("cred_modal", title="Credits", trigger="btn_credits", uiOutput("credits"))
  )

  if (is.null(inputdata())) {
    btn <- bsButton(paste0("btn_a_micro_ui_about"), label=("this button"), style="primary", size="extra-small")
    btn_pi <- bsButton(paste0("nodata_script_about"), label=("here"), style="primary", size="extra-small")

    txt_start <- paste0("To get started, you need to upload a file with microdata to the GUI. You can do so by clicking ",btn,". ")
    txt_start <- paste0(txt_start, "Alternatively, you can upload a previously saved problem instance by clicking ",btn_pi,".")
    out <- list(out, fluidRow(
      column(12, h4(strong("Getting started")), align="center"),
      column(12, p(HTML(txt_start)), align="center")
    ))
  }

  pp <- textInput("path_export_data", label=h5("Enter a directory where any exported files (data, script, problem instances) should be saved to"),
    placeholder=paste("e.g:", obj$path_export), width="50%")
  stop_btn <- bsButton("stop_sdcApp", label="Stop the GUI", style="primary", size="extra-small", onclick="setTimeout(function(){window.close();},500);")
  out <- list(out, fluidRow(
    column(12, h4(strong("Set storage path")), align="center"),
    column(12, p("Currently, all output, such as anonymized data, scripts and reports, will be saved to",code(obj$path_export),"."), align="center"),
    column(12, p("You can change the default path, where all output from the GUI will be saved. You can change this path any time
      later as well by returing to this tab."),align="center"),column(12, pp, align="center"),
    column(12, uiOutput("btn_update_export_path"), align="center"),
    column(12, h4(strong("Stop the interface")), align="center"),
    column(12, p("By clicking ", stop_btn,", you can stop the graphical user interface at any time during the anonymization process.
      If you have started the interface as",code('x <- sdcApp()'),",",code("x"),
      "will contain the micro data and the sdc problem at the state just before stopping the GUI."), align="center"),
    column(12, h4(strong("Restart the interface")), align="center"),
    column(12, p("Should you accidentally close this browser window, you can open your current problem instance by entering the local IP address
      specified in the",code("R"),"console in which you started the GUI. The address starts with", code("http://127.0.0.1:"), ". You
      can also use the refresh button of your browser in case the GUI freezes."), align="center")
  ))

  out <- list(out, fluidRow(
    column(12, h4(strong("Contact and Feedback")), align="center"),
    column(12, p("In case you have any suggestions or bug reports, please file an issue at the",
      tags$a("issue tracker", href="https://www.github.com/alexkowa/sdcMicro/issues", target="_blank"),"in our",
      tags$a("GitHub repo", href="https://www.github.com/alexkowa/sdcMicro", target="_blank"),"."), align="center"),
    column(12, p("Before reporting any bugs, please make sure that you are working with an up-to-date",tags$b("R"),"installation and
      that all packages have been updated. You can do so by entering",code("update.packages(ask=FALSE)"),"into your",code("R"),"prompt."), align="center")
  ))
  out
})
