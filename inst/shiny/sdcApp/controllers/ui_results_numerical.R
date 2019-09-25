# message if no continous key-variables have been specified
output$nonumkey_continous_results <- renderUI({
  if (!has_numkeyvars()) {
    fluidRow(
      column(12, h4("The current sdcProblem contains no",code("numerical key variables"),
        "thus no measures can be computed.", align="center")))
  }
})

# UI output displaying information on numerical risk
output$ui_resnum_numrisk_header <- renderUI({
  out <- fluidRow(
    column(12, h3("Information on risk for numerical key variables"), offset = 0, class = "wb-header"),
    column(12, p("The upper bound of the risk-interval is assumed to be 100% in the original data. Once the numeric key variables are modified,
               the upper bound may reduce. The larger the deviations from the original data, the lower the upper risk bound will be. However, this has also an
               impact on data utility."), offset = 0, class = "wb-header-hint"))
    out
})
output$ui_resnum_numrisk <- renderUI({
  if (!has_numkeyvars()) {
    return(uiOutput("nonumkey_continous_results"))
  }
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(invisible(NULL))
  }
  x <- print(curObj, type="numrisk", docat=FALSE)
  if (is.null(x)) { # no numeric key vars
    return(invisible(NULL))
  }
  out <- fluidRow(
    column(12, p("The disclosure risk in the anonymized dataset is currently between",code("0%"),"and",code(paste0(x$risk_up,"%")),"."), align="center"),
    column(12, p("In the original data the risk is assumed to be between",code("0%"),"and",code("100%"),"."), align="center"))
  out
})

# UI output displaying information about information-loss
output$ui_resnum_infoloss_header <- renderUI({
  txt1 <- paste(tags$strong("IL1"),"is the sum of the absolute distances between the corresponding observations in the raw and anonymized datasets, which")
  txt1 <- paste(txt1, "are standardized by the standard deviation of the variables in the original data.")
  out <- fluidRow(
    column(12, h3("Information-loss criteria based on numerical key variables"), offset = 0, class = "wb-header"),
    column(12, p(HTML(txt1)), offset = 0, class = "wb-header-hint"))
  out
})
output$ui_resnum_infoloss <- renderUI({

  output$ui_formula1 <- renderUI({
    withMathJax(p('$$IL1s=\\dfrac{1}{pn} \\sum_{j=1}^{p} \\sum_{i=1}^{n} \\dfrac{|x_{ij}-z_{ij}|}{\\sqrt{2} S_j}$$'))
  })
  if (!has_numkeyvars()) {
    return(uiOutput("nonumkey_continous_results"))
  }
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  x <- print(curObj, type="numrisk", docat=FALSE)

  txt1 <- paste("For the continuous variables in the dataset, the",strong("IL1s"),"measure is defined as:")

  txt2 <- paste("where",tags$i("p"),"is the number of continuous variables;",tags$i("n"),"is the number of records in the dataset;")
  txt2 <- paste(txt2,tags$i("x_ij"),"and",tags$i("z_ij"),", respectively, are the values before and after anonymization for variable",tags$i("j"))
  txt2 <- paste(txt2, "and individual",tags$i("i"),"; and",tags$i("S_j"),"is the standard deviation of variable",tags$i("j"),"in the original data (Yancey, Winkler and Creecy, 2002).")

  txt3 <- paste("The",tags$strong("difference in eigenvalues"),"is a comparison of the robust eigenvalues of the data before and after anonymization.")
  out <- fluidRow(
    column(12, p(HTML(txt1))),
    column(12, uiOutput("ui_formula1")),
    column(12, p(HTML(txt2))),
    column(12, p(HTML(txt3))),
    column(12, p("Measure",strong("IL1s"),"is",code(x$il1),"and the",strong("differences of eigenvalues"),"are",code(paste0(x$diff_eigen,"%")),"."))
  )
  out
})



# UI output displaying comparison (before-after) about numeric variables
output$ui_resnum_comparison_header <- renderUI({
  out <- fluidRow(
    column(12, h3("Compare summary statistics of numerical key variables"), offset = 0, class = "wb-header"),
    column(12, p("Here you can view the summary statistics of numeric key variables to compare the variables before and after applying anonymization methods."), offset = 0, class = "wb-header-hint")
  )
  out
})
output$ui_resnum_comparison <- renderUI({
  output$ui_numvar_numres <- renderUI({
    nv <- get_numVars_names()
    if (length(nv)==0) {
      return(NULL)
    }
    selectInput("sel_res_numvar1", label=p("Choose a numerical key variable"), choices=nv, width="100%", multiple=FALSE)
  })
  output$ui_catvar_numres <- renderUI({
    byv <- c("none", get_keyVars_names(), get_strataVar_names())
    selectInput("sel_res_catvar1", label=p("Choose a categorical variable (optional)"), choices=byv, width="100%")
  })
  output$ui_numvar_modtab <- DT::renderDataTable({
    if (is.null(input$sel_res_numvar1)) {
      return(NULL)
    }
    if (length(input$sel_res_numvar1)==0) {
      return(NULL)
    }
    df_m <- extractManipData(sdcObj())
    if (input$sel_res_catvar1!="none") {
      tab_m <- tapply(df_m[[input$sel_res_numvar1]], df_m[[input$sel_res_catvar1]], summaryfn)
      tab_m <- do.call("rbind", tab_m)
      bb <- data.frame(f=rownames(tab_m))
      colnames(bb) <- input$sel_res_catvar1
      tab_m <- cbind(bb, tab_m)
      rownames(tab_m) <- NULL
    } else {
      tab_m <- as.data.frame(t(summaryfn(df_m[[input$sel_res_numvar1]])))
    }
    tab_m
  }, options=list(scrollX=TRUE, searching=FALSE, paging=FALSE), rownames=FALSE)
  output$ui_numvar_origtab <- DT::renderDataTable({
    if (is.null(input$sel_res_numvar1)) {
      return(NULL)
    }
    if (length(input$sel_res_numvar1)==0) {
      return(NULL)
    }
    df_o <- get_origData()
    if (input$sel_res_catvar1!="none") {
      tab_o <- tapply(df_o[[input$sel_res_numvar1]], df_o[[input$sel_res_catvar1]], summaryfn)
      tab_o <- do.call("rbind", tab_o)
      bb <- data.frame(f=rownames(tab_o))
      colnames(bb) <- input$sel_res_catvar1
      tab_o <- cbind(bb, tab_o)
      rownames(tab_o) <- NULL
    } else {
      tab_o <- as.data.frame(t(summaryfn(df_o[[input$sel_res_numvar1]])))
    }
    tab_o
  }, options=list(scrollX=TRUE, searching=FALSE, paging=FALSE), rownames=FALSE)
  output$ui_numvar_cor <- renderUI({
    req(input$sel_res_numvar1)
    #if (is.null(input$sel_res_numvar1)) {
    #  return(NULL)
    #}
    if (length(input$sel_res_numvar1)==0) {
      return(NULL)
    }
    v_o <- get_origData()[[input$sel_res_numvar1]]
    v_m <- extractManipData(sdcObj())[[input$sel_res_numvar1]]
    val_cor <- round(cor(v_o, v_m, use="pairwise.complete.obs"), digits=3)
    txt_cor <- paste0("The ",strong("correlation")," between original and modified variable is ", code(val_cor),".",tags$br())

    sd_o <- round(sd(v_o, na.rm=TRUE), digits=3)
    sd_m <- round(sd(v_m, na.rm=TRUE), digits=3)
    txt_sd <- paste0("The ",strong("standard deviation")," of the original variable is ",code(sd_o)," and ", code(sd_m)," for
      the anonymized variable.", tags$br())
    iqr_o <- round(IQR(v_o, na.rm=TRUE), digits=3)
    iqr_m <- round(IQR(v_m, na.rm=TRUE), digits=3)
    txt_iqr <- paste0("The ",strong("interquartile range")," of the original variable is ",code(iqr_o)," and ", code(iqr_m)," for
      the anonymized variable.", tags$br())

    fluidRow(
      column(12, h5("Measures"), align="center"),
      column(12, p(HTML(txt_cor)), align="center"),
      column(12, p(HTML(txt_sd)), align="center"),
      column(12, p(HTML(txt_iqr)), align="center")
    )
  })
  if (!has_numkeyvars()) {
    return(uiOutput("nonumkey_continous_results"))
  }

  res <- fluidRow(
    column(6, uiOutput("ui_numvar_numres"), align="center"),
    column(6, uiOutput("ui_catvar_numres"), align="center"))

  res <- list(res, uiOutput("ui_numvar_cor"), fluidRow(
    column(12, h5("Original Data", align="center")),
    column(12, DT::dataTableOutput("ui_numvar_origtab")),
    column(12, h5("Anomyized Data", align="center")),
    column(12, DT::dataTableOutput("ui_numvar_modtab"))))
  res
})
