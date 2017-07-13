output$ui_results_main_header <- renderUI({
  out <- NULL
  val <- obj$cur_selection_results
  ## Categorical (defined in controller/ui_results_categorical.R)
  if (val=="btn_results_1") {
    return(uiOutput("ui_rescat_riskinfo_header"))
  }
  if (val=="btn_results_2") {
    return(uiOutput("ui_rescat_suda2_header"))
  }
  if (val=="btn_results_3") {
    return( uiOutput("ui_rescat_ldiv_header"))
  }

  if (val=="btn_results_4") {
    return(uiOutput("ui_rescat_mosaicplot_header"))
  }
  if (val=="btn_results_5") {
    return(uiOutput("ui_bivariate_tab_header"))
  }
  if (val=="btn_results_6") {
    return(uiOutput("ui_rescat_recodes_header"))
  }
  if (val=="btn_results_7") {
    return(uiOutput("ui_rescat_violating_kanon_header"))
  }
  ## Numerical (defined in controller/ui_results_numerical.R)
  if (val=="btn_results_8") {
    return(uiOutput("ui_resnum_comparison_header"))
  }
  if (val=="btn_results_9") {
    return(uiOutput("ui_resnum_numrisk_header"))
  }
  if (val=="btn_results_10") {
    return(uiOutput("ui_resnum_infoloss_header"))
  }
  out
})

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
  output$ui_results_menubtns <- renderUI({

    cc1 <- c("Information of risk", "Suda2 risk measure", "l-Diversity risk measure")
    cc2 <- c("Barplot/Mosaicplot", "Tabulations", "Information loss", "Obs. violating k-anon")
    cc3 <- c("Compare summary statistics", "Disclosure risk", "Information loss")

    df <- data.frame(lab=c(cc1,cc2,cc3), header=NA)
    df$header[1] <- "Risk measures"
    df$header[4] <- "Visualizations"
    df$header[8] <- "Numerical risk measures"

    out <- NULL
    for (i in 1:nrow(df)) {
      id <- paste0("btn_results_",i)
      if (obj$cur_selection_results==id) {
        style <- "primary"
      } else {
        style <- "default"
      }
      if (!is.na(df$header[i])) {
        out <- list(out, fluidRow(column(12, h4(df$header[i]), align="center")))
      }
      out <- list(out, fluidRow(
        # TODO: See issue https://github.com/skounis/sdcMicro/issues/48
        # column(12, bsButton(id, label=df$lab[i], block=TRUE, size="extra-small", style=style), tags$br())
        column(12, bsButton(id, label=df$lab[i], block=TRUE, size="extra-small", style=style))
      ))
    }
    out
  })
  # required observers that update the color of the active button!
  eval(parse(text=genObserver_menus(pat="btn_results_", n=1:10, updateVal="cur_selection_results")))
  return(uiOutput("ui_results_menubtns"))
})

output$ui_results <- renderUI({
  curObj <- sdcObj()
  if (is.null(obj$inputdata)) {
    return(list(
      noInputData(uri="ui_script"),
      fluidRow(column(12, tags$br(), p("or go to the Undo tab and upload a previously saved problem instance."), align="center")),
      fluidRow(column(12, myActionButton("nodata_script_uploadproblem", label="Upload a previously saved problem", btn.style="primary"), align="center"))
    ))
  } else if (is.null(curObj)) {
    return(list(
      noSdcProblem(uri="ui_results"),
      fluidRow(column(12, tags$br(), p("or go to the Undo tab and upload a previously saved problem instance."), align="center")),
      fluidRow(column(12, myActionButton("nodata_results_uploadproblem", label="Upload a previously saved problem", btn.style="primary"), align="center"))
    ))
  } else {
    out <- fluidRow(
      column(width = 2, uiOutput("ui_results_sidebar_left"), class="wb_sidebar"),
      list(column(width = 10, uiOutput("ui_results_main_header"), class="wb-maincolumn"),
           column(width = 7, uiOutput("ui_results_main"), class="wb-secondary-column"),
                    column(width = 3, isolate(uiOutput("sb_info_results")))))
  }
  out
})
