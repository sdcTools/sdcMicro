
choices_import <- reactive({
  if(is.null(sdcObj())) {
    return(c(
      "View the current script"="script_view",
      "Import a previously saved sdcProblem"="script_import"))
  } else {
    return(c(
      "View the current script"="script_view",
      "Import a previously saved sdcProblem"="script_import",
      "Export/Save the current sdcProblem"="script_export"))
  }
})

output$current_code <- renderText({
  code_ges <- c(obj$code, obj$code_read_and_modify, obj$code_setup, obj$code_anonymize)
  paste0("<pre class='r'><code class='r' id='codeout'>",paste(highr:::hi_html(code_ges), collapse="\n"),"</code></pre>")
})

# GUI-output to view script
output$ui_script_view <- renderUI({
  fluidRow(
    column(12, h4("View the current generated script")),
    column(12, uiOutput("current_code"))
  )
})

# GUI-output to export script
output$exportProblem_btn <- renderUI({
  req(input$path_export_problem)
  if (!dir.exists(input$path_export_problem)) {
    return(myActionButton("btn_exportProblem_xxx", "Error: The specified directory does not exist!", btn.style="danger"))
  }
  if (file.access(input$path_export_problem, mode=2)!=0) {
    return(myActionButton("btn_exportProblem_xxx", "Error: The specified directory is not writeable!", btn.style="danger"))
  }
  return(myActionButton("btn_exportProblem", "Save the current problem", btn.style="primary"))
})

observeEvent(input$report_path, {
  obj$path_export_problem <- input$path_export_problem
})
output$ui_script_export <- renderUI({
  pp <- textInput("path_export_problem", label=h5("Enter a directory where you want to write the file to"),
    placeholder=paste("e.g:",getwd()), width="75%", value=obj$path_export_problem)

  out <- fluidRow(column(12, h4("Export an existing sdcProblem", align="center")))

  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Trying to export the current problem instance resulted in the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))))
  }

  out <- list(out, fluidRow(
    column(12, p("You can save all relevant data and code for later re-use by clicking the button below.", align="center")),
    column(12, p("Note: This feature is GUI-only and cannot be reproduced from the command-line version.", align="center")),
    column(12, pp, align="center"),
    column(12, uiOutput("exportProblem_btn"), align="center")))

  if (!is.null(obj$lastproblemexport)) {
    out <- list(out, fluidRow(
      column(12, tags$br(), p("Information: the last data you have exported was saved as", code(obj$lastproblemexport)), align="center")))
  }
  out
})

# GUI-output to import previously saved sdcProblem
output$ui_script_import <- renderUI({
  cur_error <- lastError()
  btn <- myActionButton("btn_reset_inputerror2",label=("Try again!"), "primary")
  if (!is.null(lastError())) {
    return(fluidRow(
      column(12, h4("Importing previously saved sdcProblem resulted in an error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror")),
      column(12, p(btn, align="center"))
    ))
  }
  out <- fluidRow(column(12, h4("Import a previously exported sdcProblem", align="center")))
  if (!is.null(sdcObj())) {
    out <- list(out, fluidRow(column(12, p("The file must be an",code(".rdata"),"File. Please note that uploading a previously saved problem, will overwrite any existing current sdcProblem-instance!", align="center"))))
  }
  fI <- fileInput("file_importProblem", strong("Select previously exported sdcProblem (.rdata)"), width="100%", accept=".rdata")
  out <- list(out, fluidRow(column(12, p(fI, align="center"))))
  out
})

output$ui_script_main <- renderUI({
  if (!is.null(input$sel_script)) {
    if (input$sel_script=="script_view") {
      return(uiOutput("ui_script_view"))
    }
    if (input$sel_script=="script_import") {
      return(uiOutput("ui_script_import"))
    }
    if (input$sel_script=="script_export") {
      return(uiOutput("ui_script_export"))
    }
  }
})

output$ui_script_sidebar_left <- renderUI({
  rb <- radioButtons("sel_script", label=h4("What do you want to do?", align="center"),
  choices=choices_import(), selected=input$sel_script, width="100%")
  fluidRow(column(12, rb))
})

output$ui_script <- renderUI({
  if (is.null(obj$inputdata)) {
    return(noInputData(uri="ui_script"))
  } else {
    out <- fluidRow(
      column(3, uiOutput("ui_script_sidebar_left")),
      column(9, uiOutput("ui_script_main")))
  }
  return(out)
})

