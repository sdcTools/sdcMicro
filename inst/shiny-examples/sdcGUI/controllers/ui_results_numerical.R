# message if no continous key-variables have been specified
output$nonumkey_continous_results <- renderUI({
  if (!has_numkeyvars()) {
    fluidRow(
      column(12, h4("The current sdcProblem contains no",code("numerical key variables"),
        "thus no measures can be computed.", align="center")))
  }
})

# display information on numerical risk
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
    column(12, h4("Information on Risk for numerical key-variables"), align="center"),
    column(12, p("The upper bound of the risk-interval is assumed to be 100% in the original data. Once the numeric key variables are modified,
      the upper bound may reduce. The larger the deviations from the original data, the lower the upper risk bound will be. However, this has also an
      impact on data utility."),align="center"),
    column(12, p("The disclosure risk in the anonymized dataset is currently between",code("0%"),"and",code(paste0(x$risk_up,"%")),"."), align="center"),
    column(12, p("In the original data the risk is assumed to be between",code("0%"),"and",code("100%"),"."), align="center"))
  out
})

# display information about information-loss
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

  txt1 <- paste(tags$strong("IL1"),"is the sum of the absolute distances between the corresponding observations in the raw and anonymized datasets, which")
  txt1 <- paste(txt1, "are standardized by the standard deviation of the variables in the original data. For the continuous variables in the dataset, the")
  txt1 <- paste(txt1, "IL1 measure is defined as:")

  txt2 <- paste("where",tags$i("p"),"is the number of continuous variables;",tags$i("n"),"is the number of records in the dataset;")
  txt2 <- paste(txt2,tags$i("x_ij"),"and",tags$i("z_ij"),", respectively, are the values before and after anonymization for variable",tags$i("j"))
  txt2 <- paste(txt2, "and individual",tags$i("i"),"; and",tags$i("S_j"),"is the standard deviation of variable",tags$i("j"),"in the original data (Yancey, Winkler and Creecy, 2002).")

  #where  is the number of continuous variables;  is the number of records in the dataset;  and , respectively, are the values before and after anonymization for variable  and individual ; and  is the standard deviation of variable  in the original data (Yancey, Winkler and Creecy, 2002).#

  txt3 <- paste("The",tags$strong("difference in eigenvalues"),"is a comparison of the robust eigenvalues of the data before and after anonymization.")
  out <- fluidRow(
    column(12, h4("Information-Loss criteria based on numerical key-variables"), align="center"),
    column(12, p(HTML(txt1)), align="center"),
    column(12, uiOutput("ui_formula1"),align="center"),
    column(12, p(HTML(txt2)), align="center"),
    column(12, p(HTML(txt3)), align="center"),
    column(12, p("Measure",strong("IL1s"),"is",code(x$il1),"and the",strong("differences of eigenvalues"),"are",code(paste0(x$diff_eigen,"%")),"."), align="center")
  )
  out
})

output$ui_numvar_numres <- renderUI({
  nv <- get_numVars_names()
  if (length(nv)==0) {
    return(NULL)
  }
  selectInput("sel_res_numvar1",label=h5("Numerical key Variable"),
    choices=nv, selected=input$sel_res_numvar1, width="100%", multiple=FALSE)
})
output$ui_catvar_numres <- renderUI({
  byv <- c("none", get_keyVars_names(), get_strataVar_names())
  selectInput("sel_res_catvar1",label=h5("by Variable"), choices=byv,
    selected=input$sel_res_catvar1, width="100%")
})


# display comparison (before-after) about numeric variables
output$ui_resnum_comparison <- renderUI({
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
  }, options=list(scrollX=TRUE, searching=FALSE, paging=FALSE))
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
  }, options=list(scrollX=TRUE, searching=FALSE, paging=FALSE))
  output$ui_numvar_cor <- renderUI({
    if (is.null(input$sel_res_numvar1)) {
      return(NULL)
    }
    if (length(input$sel_res_numvar1)==0) {
      return(NULL)
    }
    v_o <- get_origData()[[input$sel_res_numvar1]]
    v_m <- extractManipData(sdcObj())[[input$sel_res_numvar1]]
    vv <- round(cor(v_o, v_m), digits=3)
    txt_cor <- paste0("The ",strong("correlation")," between original and modified variable is", code(vv),". The ",strong("standard deviation"),
    " of the original variable is ",code(round(sd(v_o),digits=3))," and the ",strong("standard deviation")," of the
    modified variable is ", code(round(sd(v_m),digits=3)),".")
    fluidRow(column(12, p(HTML(txt_cor), align="center")))
  })
  if (!has_numkeyvars()) {
    return(uiOutput("nonumkey_continous_results"))
  }

  res <- fluidRow(
    column(6, uiOutput("ui_numvar_numres")),
    column(6, uiOutput("ui_catvar_numres")))

  res <- list(res, uiOutput("ui_numvar_cor"), fluidRow(
    column(12, h5("Original Values", align="center")),
    column(12, dataTableOutput("ui_numvar_origtab")),
    column(12, h5("Modified Data", align="center")),
    column(12, dataTableOutput("ui_numvar_modtab"))))
  res
})
