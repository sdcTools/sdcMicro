output$current_code <- renderText({
  code_ges <- c(obj$code, obj$code_read_and_modify, obj$code_anonymize)
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
  htmlTemplate("tpl_one_col.html", inp=h4("Export Script"))
})

# GUI-output to import script
output$ui_script_import <- renderUI({
  htmlTemplate("tpl_one_col.html", inp=h4("Import Script"))
})

output$ui_script <- renderUI({
  btn <- selectInput("sel_script", label=h5("What do you want to do?"),
    choices=c("View the current script"="script_view","Import a previously saved script"="script_import","Export/Save the current script"="script_export"),
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
