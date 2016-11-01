# GUI-output to export script
output$ui_export_problem1 <- renderUI({
  out <- fluidRow(column(12, h4("Export an existing sdcProblem", align="center")))
  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Trying to export the current problem instance resulted in the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))))
  }
  out <- list(out, fluidRow(
    column(12, p("You can save all relevant data and code for later re-use by clicking the button below.", align="center")),
    column(12, p("Note: This feature is GUI-only and cannot be reproduced from the command-line version.", align="center")),
    column(12, myActionButton("btn_exportProblem1", "Save the current problem", btn.style="primary"), align="center")))

  if (!is.null(obj$lastproblemexport1)) {
    out <- list(out, fluidRow(
      column(12, tags$br(), p("Information: the last data you have exported was saved as", code(obj$lastproblemexport1)), align="center")))
  }
  out
})

# GUI-output to import previously saved sdcProblem
output$ui_script_import1 <- renderUI({
  cur_error <- lastError()
  btn <- myActionButton("btn_reset_inputerror3",label=("Try again!"), "primary")
  if (!is.null(lastError())) {
    return(fluidRow(
      column(12, h4("Importing previously saved sdcProblem resulted in an error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror")),
      column(12, btn, align="center")
    ))
  }
  out <- fluidRow(column(12, h4("Import a previously exported sdcProblem", align="center")))
  if (!is.null(sdcObj())) {
    out <- list(out, fluidRow(column(12, p("The file must be an",code(".rdata"),"File. Please note that uploading a previously saved problem, will overwrite any existing current sdcProblem-instance!", align="center"))))
  }
  fI <- fileInput("file_importProblem1", strong("Select previously exported sdcProblem (.rdata)"), width="50%", accept=".rdata")
  out <- list(out, fluidRow(column(12, fI, align="center")))
  out
})


output$ui_undo <- renderUI({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(noSdcProblem(uri="ui_undo"))
  }
  if (is.null(curObj@prev)) {
    out <- fluidRow(
      column(12, h4("Undo last step"), align="center"),
      column(12, p("Currently, no step can be undone."), align="center"))
    out <- list(out, uiOutput("ui_export_problem1"), uiOutput("ui_script_import1"))
    return(out)
  } else {
    btn_undo <- myActionButton("btn_undo", "Undo last Step", "danger", css.class="btn-xs")
    out <- fluidRow(
      column(12, h4("Undo last step"), align="center"),
      column(12, p("Clicking the button below will remove (if possible) the following anonymization step!"), align="center"),
      column(12, list(code(obj$lastaction), br(), br()), align="center"),
      column(12, btn_undo, align="center"))
    out <- list(out, uiOutput("ui_export_problem1"), uiOutput("ui_script_import1"))
    return(out)
  }
})
