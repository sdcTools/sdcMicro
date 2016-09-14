
choices_import <- reactive({
  if(is.null(obj$sdcObj)) {
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
output$ui_script_export <- renderUI({
  db <- downloadButton('exportProblem', 'Download current sdcProblem (and code) to disk')
  fluidRow(
    column(12, h4("Export an existing sdcProblem", align="center")),
    column(12, p("You can now download all relevant data and code for later re-use by clicking the button below.", align="center")),
    column(12, p(db, align="center")))
})

output$exportProblem <- downloadHandler(
  filename=function() {
    paste0("sdcProblem_GUI_export",format(Sys.time(), "%Y%m%d_%H%M"),".rdata")
  },
  content=function(file) {
    prob <- reactiveValuesToList(obj, all.names=FALSE)
    class(prob) <- "sdcMicro_GUI_export"
    save(prob, file=file, compress=TRUE)
  }
)

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
  if (!is.null(obj$sdcObj)) {
    out <- list(out, fluidRow(column(12, p("Take care. When you upload a previously saved problem, this will overwrite any existing sdcProblem-instance!", align="center"))))
  }
  fI <- fileInput("file_importProblem", strong("Select previously exported sdcProblem"), width="100%", accept=".rdata")
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
  } else if (is.null(obj$sdcObj)) {
    out <- fluidRow(
      column(3, uiOutput("ui_script_sidebar_left")),
      column(9, uiOutput("ui_script_main")))
    } else {
    out <- fluidRow(
      column(3, uiOutput("ui_script_sidebar_left")),
      column(6, uiOutput("ui_script_main")),
      column(3, uiOutput("sb_info_script")))
  }
  return(out)
})

