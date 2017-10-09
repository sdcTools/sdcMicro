# GUI-output to export script
output$ui_export_problem1 <- renderUI({
  out <- fluidRow(column(12, h4("Save and retrieve current state")))
  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Trying to export the current problem instance resulted in the following error!")),
      column(12, verbatimTextOutput("ui_lasterror")), class = "wb-error-toast"))
  }
  out <- list(out, fluidRow(
    column(12, p("The undo button can only be used to go one step back. For experimenting with SDC methods, parameters and settings, it can be
      useful to save a certain state before starting to experiment with different SDC methods and, if the result is not satisfactory, revert to the
      saved state. Here you can save the current state and, if necessary, reload this state. Reloading undoes any methods applied to the data since
      saving the last state, but restores any methods applied before the saving. It is also possible to save several states, as they are saved on
      disk.")),
    column(12, p("Note: This feature is GUI-only and cannot be reproduced from the command-line version.")),
    column(12, h5("Save current state")),
    column(12, p("Click here to save the current state with all relevant data and code for reverting to this state later. This can also be used to save
      the current state and continue working on this SDC problem at a later point in time.")),
    column(12, myActionButton("btn_exportProblem1", "Save current state", btn.style="primary"), align="center")))

  if (!is.null(obj$lastproblemexport1)) {
    out <- list(out, fluidRow(
      column(12, tags$br(), p("The last saved state was saved under the following path as:", code(obj$lastproblemexport1)))))
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
  if (is.null(sdcObj())) {
    out <- fluidRow(column(12, h5("Load a previously saved problem instance")))
    out <- list(out, fluidRow(column(12, p("Here you can load a previously saved problem instance. The file must be an",code(".rdata"),"file."))))
  }
  if (!is.null(sdcObj())) {
    out <- fluidRow(column(12, h5("Revert to saved state")))
    out <- list(out, fluidRow(column(12, p("Here you can load a previously saved state. The file must be an",code(".rdata"),"file. See above for the
      path where you saved the last state. Please note that uploading a previously saved state overwrites all current results and results into a loss of any unsaved changes!"))))
  }
  fI <- fileInput("file_importProblem1", strong("Select previously exported sdcProblem (.rdata)"), width="50%", accept=".rdata")
  out <- list(out, fluidRow(column(12, fI, align="center")))
  out
})


output$ui_undo_main <- renderUI({
  if (is.null(inputdata())) {
    return(list(noInputData(uri="ui_undo"),
      fluidRow(column(12, tags$br(), p("or"), align="center")), uiOutput("ui_script_import1") ))
  }
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(list(noSdcProblem(uri="ui_undo"),
      fluidRow(column(12, tags$br(), p("or"), align="center")), uiOutput("ui_script_import1")))
  }
  if (is.null(curObj@prev)) {
    out <- fluidRow(
      column(12, h3("Undo last step"), class="wb-header"),
      column(12, p("Currently, no step can be undone."), class="wb-header-hint")
    )
    out <- list(out, uiOutput("ui_export_problem1"), uiOutput("ui_script_import1"))
    return(out)
  } else {
    btn_undo <- myActionButton("btn_undo_xx", "Undo last Step", "danger")
    out <- fluidRow(
      column(12, h3("Undo last step"), class="wb-header"),
      column(12, p("Clicking the button below will remove (if possible) the following anonymization step!"), class="wb-header-hint"),
      column(12, list(code(obj$lastaction), br(), br()), align="center"),
      column(12, btn_undo, align="center"))
    out <- list(out, uiOutput("ui_export_problem1"), uiOutput("ui_script_import1"))
    return(out)
  }
})

output$ui_undo <- renderUI({
  return(fluidRow(
    column(12, uiOutput("ui_undo_main"), class="wb-maincolumn")))
})
