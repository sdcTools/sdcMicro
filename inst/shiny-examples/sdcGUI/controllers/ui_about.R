output$ui_about <- renderUI({
  fluidRow(
    column(12, h4("About the Interface", align="center")),
    column(12, p("Some information about",code("sdcMicro"),"and how to use this graphical interface."), align="center"),
    column(12, h4("Help"), align="center"),
    column(12, p("Link to GUI-Tutorial"), align="center"),
    column(12, p("Link to GUI-Tutorial"), align="center"),
    column(12, p("If you already have an sdcProblem that was exported from the GUI, you can upload it in Tab",code("Reproducibility"),"."), align="center")
  )
})
