output$ui_undo <- renderUI({
  btn <- myActionButton("b_undo", "Undo last Step", "danger")
  list(
    htmlTemplate("tpl_one_col.html", inp=h2("Undo last step")),
    htmlTemplate("tpl_one_col.html", inp=btn)
  )
})
