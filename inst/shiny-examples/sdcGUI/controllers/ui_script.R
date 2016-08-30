output$current_code <- renderText({
  code_ges <- c(obj$code, obj$code_read_and_modify, obj$code_setup, obj$code_anonymize)
  paste0("<pre class='r'><code class='r' id='codeout'>",paste(highr:::hi_html(code_ges), collapse="\n"),"</code></pre>")
})

# GUI-output to view script
output$ui_script_view <- renderUI({
  out <- list(
    htmlTemplate("tpl_one_col.html", inp=h4("View Script")),
    htmlTemplate("tpl_one_col_code.html", inp=uiOutput("current_code"))
  )
  out
})

# GUI-output to export script
output$ui_script_export <- renderUI({
  db <- downloadButton('exportProblem', 'Download current sdcProblem (and code) to disk')
  out <- list(htmlTemplate("tpl_one_col.html", inp=h4("Export sdcProblem")))
  if (!is.null(obj$sdcObj)) {
    out <- list(out, htmlTemplate("tpl_one_col.html", inp=p("You can now download all relevant data and code for later re-use by clicking the button below.")))
    out <- list(out, htmlTemplate("tpl_one_col.html", inp=db))
  } else {
    out <- list(out, htmlTemplate("tpl_one_col.html", inp=p("Currently, there is no",code("sdcProblem"),"defined")))
  }
  out
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
    return(list(
      htmlTemplate("tpl_one_col.html", inp=h4("Importing previously saved sdcProblem resulted in an error!")),
      htmlTemplate("tpl_one_col.html", inp=verbatimTextOutput("ui_lasterror")),
      htmlTemplate("tpl_one_col.html", inp=btn)))
  }

  out <- htmlTemplate("tpl_one_col.html", inp=h2("Import a previously exported sdcProblem"))
  if (!is.null(obj$sdcObj)) {
    out <- list(out, htmlTemplate("tpl_one_col.html", inp=p("Take care, your current sdcProblem will be overwritten!")))
  }
  fI <- fileInput("file_importProblem", h4("Select previously exported sdcProblem"), width="100%", accept=".rdata")
  out <- list(out, htmlTemplate("tpl_one_col.html", inp=fI))
  out
})

output$ui_script <- renderUI({
  btn <- selectInput("sel_script", label=h5("What do you want to do?"),
    choices=c(
      "View the current script"="script_view",
      "Import a previously saved sdcProblem"="script_import",
      "Export/Save the current sdcProblem"="script_export"),
    selected=input$sel_script, width="100%")
  out <- list(
    htmlTemplate("tpl_one_col.html",inp=h2("Reproducibility")),
    htmlTemplate("tpl_one_col.html",inp=btn)
  )
  if ( !is.null(input$sel_script) ) {
    if ( input$sel_script=="script_view" ) {
      out <- list(out, uiOutput("ui_script_view"))
    }
    if ( input$sel_script=="script_import" ) {
      out <- list(out, uiOutput("ui_script_import"))
    }
    if ( input$sel_script=="script_export" ) {
      out <- list(out, uiOutput("ui_script_export"))
    }
  }
  out
})
