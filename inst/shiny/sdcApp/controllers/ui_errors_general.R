output$ui_lastwarning <- renderPrint({
  cat(lastWarning())
})

output$ui_lasterror <- renderPrint({
  cat(lastError())
})
