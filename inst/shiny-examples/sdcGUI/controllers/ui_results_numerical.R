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
  fluidRow(column(12, h4("Information about numerical risk", align="center")))
})

# display information about information-loss
output$ui_resnum_infoloss <- renderUI({
  if (!has_numkeyvars()) {
    return(uiOutput("nonumkey_continous_results"))
  }
  fluidRow(column(12, h4("Information about information loss", align="center")))
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
