output$ui_results_main <- renderUI({
  out <- NULL
  val <- obj$cur_selection_results
  ## Categorical (defined in controller/ui_results_categorical.R)
  if (val=="btn_results_1") {
    return(uiOutput("ui_rescat_riskinfo"))
  }
  if (val=="btn_results_2") {
    return(uiOutput("ui_rescat_suda2"))
  }
  if (val=="btn_results_3") {
    return( uiOutput("ui_rescat_ldiv"))
  }

  if (val=="btn_results_4") {
    return(uiOutput("ui_rescat_mosaicplot"))
  }
  if (val=="btn_results_5") {
    return(uiOutput("ui_bivariate_tab"))
  }
  if (val=="btn_results_6") {
    return(uiOutput("ui_rescat_recodes"))
  }
  if (val=="btn_results_7") {
    return(uiOutput("ui_rescat_violating_kanon"))
  }
  ## Numerical (defined in controller/ui_results_numerical.R)
  if (val=="btn_results_8") {
    return(uiOutput("ui_resnum_comparison"))
  }
  if (val=="btn_results_9") {
    return(uiOutput("ui_resnum_numrisk"))
  }
  if (val=="btn_results_10") {
    return(uiOutput("ui_resnum_infoloss"))
  }
  out
})

output$ui_results_sidebar_left <- renderUI({
  output$ui_sel_resbtns_cat <- renderUI({
    fluidRow(
      column(12, h4("Risk measures"), align="center"),
      column(12, bsButton("btn_results_1", "Information of risks", block=TRUE, size="extra-small", style="primary"), tags$br()),
      column(12, bsButton("btn_results_2", "Suda2 risk-measure", block=TRUE, size="extra-small", style="default"), tags$br()),
      column(12, bsButton("btn_results_3", "l-Diversity risk-measure", block=TRUE, size="extra-small", style="default"), tags$br())
    )
  })
  output$ui_sel_resbtns_vis <- renderUI({
    fluidRow(
      column(12, h4("Visualizations"), align="center"),
      column(12, bsButton("btn_results_4", "Histogram/Mosaicplot", block=TRUE, size="extra-small", style="default"), tags$br()),
      column(12, bsButton("btn_results_5", "Tabulations", block=TRUE, size="extra-small", style="default"), tags$br()),
      column(12, bsButton("btn_results_6", "Information Loss", block=TRUE, size="extra-small", style="default"), tags$br()),
      column(12, bsButton("btn_results_7", "Obs violating k-Anon", block=TRUE, size="extra-small", style="default"), tags$br())
    )
  })
  output$ui_sel_resbtns_num <- renderUI({
    fluidRow(
      column(12, h4("Numerical Risk Measures"), align="center"),
      column(12, bsButton("btn_results_8", "Compare summary statistics", block=TRUE, size="extra-small", style="default"), tags$br()),
      column(12, bsButton("btn_results_9", "Disclosure Risk", block=TRUE, size="extra-small", style="default"), tags$br()),
      column(12, bsButton("btn_results_10", "Information Loss", block=TRUE, size="extra-small", style="default"), tags$br())
    )
  })

  # required observers that update the color of the active button!
  eval(parse(text=genObserver_menus(pat="btn_results_", n=1:10, updateVal="cur_selection_results")))

  return(list(uiOutput("ui_sel_resbtns_cat"), uiOutput("ui_sel_resbtns_vis"), uiOutput("ui_sel_resbtns_num")))
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
