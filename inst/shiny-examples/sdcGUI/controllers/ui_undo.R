output$ui_undo <- renderUI({
  if (is.null(sdcObj())) {
    return(noSdcProblem(uri="ui_undo"))
  }
  if (is.null(sdcObj()@prev)) {
    return(fluidRow(
      column(12, h4("Undo last step", align="center")),
      column(12, p("Currently, no step can be undone.", align="center"))))
  } else {
    btn_undo <- myActionButton("btn_undo", "Undo last Step", "danger", css.class="btn-xs")

    return(fluidRow(
      column(12, h4("Undo last step", align="center")),
      column(12, p("Clicking the button below will remove (if possible) the following anonymization step!", align="center")),
      column(12, list(code(obj$lastaction), br(), br()), align="center"),
      column(12, p(btn_undo, align="center"))))
  }
})
