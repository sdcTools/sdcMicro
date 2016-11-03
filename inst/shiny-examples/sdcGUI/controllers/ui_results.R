output$ui_results_main <- renderUI({
  out <- NULL
  if (!is.null(input$sel_results)) {
    ## Categorical (defined in controller/ui_results_categorical.R)
    if (input$sel_results=="riskinfo") {
      out <- list(out, uiOutput("ui_rescat_riskinfo"))
    }
    if (input$sel_results=="ldiv") {
      out <- list(out, uiOutput("ui_rescat_ldiv"))
    }
    if (input$sel_results=="suda2") {
      out <- list(out, uiOutput("ui_rescat_suda2"))
    }
    if (input$sel_results=="mosaicplot") {
      out <- list(out, uiOutput("ui_rescat_mosaicplot"))
    }
    if (input$sel_results=="tabulations") {
      out <- list(out, uiOutput("ui_bivariate_tab"))
    }
    if (input$sel_results=="recodes") {
      out <- list(out, uiOutput("ui_rescat_recodes"))
    }
    if (input$sel_results=="riskyobs") {
      out <- list(out, uiOutput("ui_rescat_riskyobs"))
    }
    if (input$sel_results=="violating_kanon") {
      out <- list(out, uiOutput("ui_rescat_violating_kanon"))
    }
    ## Numerical (defined in controller/ui_results_numerical.R)
    if (input$sel_results=="numrisk") {
      out <- list(out, uiOutput("ui_resnum_numrisk"))
    }
    if (input$sel_results=="infoloss") {
      out <- list(out, uiOutput("ui_resnum_infoloss"))
    }
    if (input$sel_results=="comparison") {
      out <- list(out, uiOutput("ui_resnum_comparison"))
    }
  }
  out
})

output$ui_results_sidebar_left <- renderUI({
  # choices for dropdown-menus in tab 'View/Analyse results'
  choices_results <- reactive({
    if (is.null(input$rb_results_type)) {
      return(NULL)
    }
    if (input$rb_results_type=="res_risk") {
      choices <- c(
        "Information of Risks"="riskinfo",
        "Suda2 risk-measure"="suda2",
        "l-Diversity risk-measure"="ldiv")
    }
    if (input$rb_results_type=="res_vis") {
      choices <- c(
        "Histogram/Mosaicplot"="mosaicplot",
        "Tabulations"="tabulations",
        "Information Loss"="recodes",
        "Obs violating k-Anon"="violating_kanon")
    }
    if (input$rb_results_type=="res_numrisk") {
      choices <- c(
        "Compare summary statistics"="comparison",
        "Disclosure Risk"="numrisk",
        "Information Loss"="infoloss")
    }
    choices
  })
  output$ui_sel_results <- renderUI({
    if (is.null(input$sel_results)) {
      sel <- choices_results()[1]
    } else {
      sel <- input$sel_results
    }
    radioButtons("sel_results", label=h5("Choose a measure/result"),
      choices=choices_results(),selected=sel, width="100%")
  })
  rb1 <- radioButtons("rb_results_type", label=h5("What kind of results do you want to display?"),
    choices=c("Risk measures"="res_risk", "Visualizations"="res_vis", "Numerical Risk Measures"="res_numrisk"),
    selected=input$rb_results_type, inline=FALSE)
  fluidRow(
    column(12, rb1),
    column(12, uiOutput("ui_sel_results")))
})

output$ui_results <- renderUI({
  if (is.null(sdcObj())) {
    return(list(
      noSdcProblem(uri="ui_results"),
      fluidRow(column(12, tags$br(), p("or go back to tab 'Undo' and upload a previously saved problem instance"), align="center")),
      fluidRow(column(12, myActionButton("nodata_results_uploadproblem", label="Upload a previously saved problem", btn.style="primary"), align="center"))
    ))
  } else {
    out <- fluidRow(
      column(2, uiOutput("ui_results_sidebar_left")),
      column(7, uiOutput("ui_results_main")),
      column(3, uiOutput("sb_info_results")))
  }
  out
})
