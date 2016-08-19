output$ui_results <- renderUI({
  # choices for dropdown-menus in tab 'View/Analyse results'
  choices_results <- reactive({
    if (is.null(input$rb_results_type)) {
      return(NULL)
    }
    if (input$rb_results_type=="res_cat") {
      choices <- c(
        "Risk"="freqcalc",
        "Risky Observations"="riskyobs",
        "l-Diversity risk-measure"="ldiv",
        "Suda2 risk-measure"="suda2",
        "View Histogram/Mosaicplot"="mosaicplot",
        "View (bivariate) Tabulations"="tabulations",
        "Information Loss (Recodings)"="recodes",
        "Obs violating k-Anon"="violating_kanon")
    }
    if (input$rb_results_type=="res_num") {
      choices <- c(
        "Disclosure Risk"="numrisk",
        "Information Loss"="infoloss",
        "Compare original vs. modified"="comparison")
    }
    choices
  })

  output$ui_sel_results <- renderUI({
    if (is.null(input$sel_results)) {
      sel <- choices_results()[1]
    } else {
      sel <- input$sel_results
    }
    selectInput("sel_results", label=h5("Choose a measure/result"),
      choices=choices_results(),selected=sel, width="100%")
  })

  if (is.null(obj$sdcObj)) {
    return(noSdcProblem(uri="ui_results"))
  }

  rb1 <- radioButtons("rb_results_type", label=h5("What kind of results do you want to display?"),
   choices=c("Info about categorical variables"="res_cat",
             "Info about numerical variables"="res_num"),
   selected=input$rb_results_type, inline=TRUE)
  out <- list(
    htmlTemplate("tpl_one_col.html",inp=h2("Results and Measures")),
    htmlTemplate("tpl_two_col.html",inp1=rb1, inp2=uiOutput("ui_sel_results"))
  )
  if ( !is.null(input$sel_results) ) {
    ## Categorical (defined in controller/ui_results_categorical.R)
    if ( input$sel_results=="freqcalc" ) {
      out <- list(out, uiOutput("ui_rescat_freqCalc"))
    }
    if ( input$sel_results=="ldiv" ) {
      out <- list(out, uiOutput("ui_rescat_ldiv"))
    }
    if ( input$sel_results=="suda2" ) {
      out <- list(out, uiOutput("ui_rescat_suda2"))
    }
    if ( input$sel_results=="mosaicplot" ) {
      out <- list(out, uiOutput("ui_rescat_mosaicplot"))
    }
    if ( input$sel_results=="tabulations" ) {
      out <- list(out, uiOutput("ui_bivariate_tab"))
    }
    if ( input$sel_results=="recodes" ) {
      out <- list(out, uiOutput("ui_rescat_recodes"))
    }
    if ( input$sel_results=="riskyobs" ) {
      out <- list(out, uiOutput("ui_rescat_riskyobs"))
    }
    if ( input$sel_results=="violating_kanon" ) {
      out <- list(out, uiOutput("ui_rescat_violating_kanon"))
    }
    ## Numerical (defined in controller/ui_results_numerical.R)
    if ( input$sel_results=="numrisk" ) {
      out <- list(out, uiOutput("ui_resnum_numrisk"))
    }
    if ( input$sel_results=="infoloss" ) {
      out <- list(out, uiOutput("ui_resnum_infoloss"))
    }
    if ( input$sel_results=="comparison" ) {
      out <- list(out, uiOutput("ui_resnum_comparison"))
    }
  }
  out
})
