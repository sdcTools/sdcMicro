output$ui_undo <- renderUI({
  btn_undo <- myActionButton("btn_undo", "Undo last Step", "danger")
  list(
    htmlTemplate("tpl_one_col.html", inp=h2("Undo last step")),
    htmlTemplate("tpl_one_col.html", inp=btn_undo)
  )
})
