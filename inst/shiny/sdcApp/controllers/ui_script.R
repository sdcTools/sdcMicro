choices_import <- reactive({
  if(is.null(sdcObj())) {
    return(c(
      "View/Save the current script"="script_view",
      "Import a previously saved sdcProblem"="script_import"))
  } else {
    return(c(
      "View the current script"="script_view",
      "Import a previously saved sdcProblem"="script_import",
      "Export/Save the current sdcProblem"="script_export"))
  }
})

current_code <- reactive({
  code_ges <- c(obj$code, obj$code_read_and_modify, obj$code_setup, obj$code_anonymize)
  code_ges
})

output$current_code <- renderText({
  paste0("<pre class='r'><code class='r' id='codeout'>",paste(highr:::hi_html(current_code()), collapse="\n"),"</code></pre>")
})

# GUI-output to view script
output$ui_script_view <- renderUI({
  out <- fluidRow(
    column(12, h4("View the current generated script"), align="center"),
    column(12, myActionButton("btn_save_script", "Save Script to File", btn.style="primary"), align="center"),
    column(12, tags$br(), uiOutput("current_code"))
  )

  if (!is.null(obj$lastscriptexport)) {
    out <- list(out, fluidRow(
      column(12, tags$br(), p("Information: the last script you have exported was saved as", code(obj$lastscriptexport)), align="center")))
  }
  out
})

# GUI-output to export script
output$ui_script_export <- renderUI({
  out <- fluidRow(column(12, h4("Export an existing sdcProblem", align="center")))
  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Trying to export the current problem instance resulted in the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))))
  }

  out <- list(out, fluidRow(
    column(12, p("You can save all relevant data and code for later re-use by clicking the button below.", align="center")),
    column(12, p("Note: This feature is GUI-only and cannot be reproduced from the command-line version.", align="center")),
    column(12, myActionButton("btn_exportProblem", "Save the current problem", btn.style="primary"), align="center")))

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
  fI <- fileInput("file_importProblem", strong("Select previously exported sdcProblem (.rdata)"), width="50%", accept=".rdata")
  out <- list(out, fluidRow(column(12, fI, align="center")))
  out
})

output$ui_script_main <- renderUI({
  val <- obj$cur_selection_script
  if (val=="btn_export_script_1") {
    return(uiOutput("ui_script_view"))
  }
  if (val=="btn_export_script_2") {
    return(uiOutput("ui_script_import"))
  }
  if (val=="btn_export_script_3") {
    return(uiOutput("ui_script_export"))
  }
})

output$ui_script_sidebar_left <- renderUI({
  output$ui_sel_script_btns <- renderUI({
    cc <- choices_import()
    out <- fluidRow(column(12, h4("What do you want to do?"), align="center"))
    for (i in 1:length(cc)) {
      if (i==1) {
        style <- "primary"
      } else {
        style <- "default"
      }
      out <- list(out, fluidRow(
        column(12, bsButton(paste0("btn_export_script_",i), label=names(cc)[i], block=TRUE, size="extra-small", style=style), tags$br())))
    }
    # required observers that update the color of the active button!
    eval(parse(text=genObserver_menus(pat="btn_export_script_", n=1:3, updateVal="cur_selection_script")))
    out
  })

  return(uiOutput("ui_sel_script_btns"))
})

output$ui_script <- renderUI({
  if (is.null(obj$inputdata)) {
    return(list(
      noInputData(uri="ui_script"),
      fluidRow(column(12, tags$br(), p("or go back to tab 'Undo' and upload a previously saved problem instance"), align="center")),
      fluidRow(column(12, myActionButton("nodata_script_uploadproblem", label="Upload a previously saved problem", btn.style="primary"), align="center"))
    ))
  } else {
    out <- fluidRow(
      column(2, uiOutput("ui_script_sidebar_left")),
      column(10, uiOutput("ui_script_main")))
  }
  return(out)
})

