output$ui_undo <- renderUI({
  btn_undo <- myActionButton("btn_undo", "Undo last Step", "danger")

  fluidRow(
    column(12, h4("Undo last step", align="center")),
    column(12, p("Clicking the button below will remove (if possible) the last anonymization step!", align="center")),
    column(12, p(btn_undo, align="center"))
  )
})
