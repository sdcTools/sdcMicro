output$ui_lastwarning <- renderPrint({
  cat(lastWarning()$warnMsg)
})

output$ui_lasterror <- renderPrint({
  cat(lastError())
})
