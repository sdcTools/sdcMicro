library(shiny)
shinyUI(
  navbarPage(id="mainnav", theme=.guitheme,"sdcMicro GUI",
    tabPanel("About/Help", uiOutput("ui_about")),
    tabPanel("Microdata", uiOutput("ui_inputdata")),
    tabPanel("Anonymize", uiOutput("ui_anonymize")),
    tabPanel("Risk/Utility", uiOutput("ui_results")),
    tabPanel("Export Data", uiOutput("ui_export")),
    tabPanel("Reproducibility", uiOutput("ui_script")),
    tabPanel("Undo", uiOutput("ui_undo")),
    tags$head(tags$script(src=.guijsfile))
    )
)
