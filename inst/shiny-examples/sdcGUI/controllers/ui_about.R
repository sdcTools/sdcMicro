output$ui_about <- renderUI({
  txt <- list("Some information about",code("sdcMicro"),"and how to use this
    graphical interface.")
  list(
    htmlTemplate("tpl_one_col.html", inp=h2("About the Interface")),
    htmlTemplate("tpl_one_col.html", inp=txt),
    htmlTemplate("tpl_one_col.html", inp=h2("Help")),
    htmlTemplate("tpl_one_col.html", inp="Link to GUI-Tutorial"),
    htmlTemplate("tpl_one_col.html", inp="Link to SDC-Tutorial")
  )
})
