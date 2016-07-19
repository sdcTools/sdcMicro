# message if no continous key-variables have been specified
output$nonumkey_continous_results <- renderUI({
  if (!has_numkeyvars()) {
    return(htmlTemplate("tpl_one_col.html", inp=list(
      h4("The current sdcProblem contains no",code("numerical key variables"),
         "thus no measures can be computed.")
    )))
  }
})

# display information on numerical risk
output$ui_resnum_numrisk <- renderUI({
  if (!has_numkeyvars()) {
    return(uiOutput("nonumkey_continous_results"))
  }
  return(htmlTemplate("tpl_one_col.html",inp=h4("Information about numerical risk")))
})

# display information about information-loss
output$ui_resnum_infoloss <- renderUI({
  if (!has_numkeyvars()) {
    return(uiOutput("nonumkey_continous_results"))
  }
  return(htmlTemplate("tpl_one_col.html",inp=h4("Information about information loss")))
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
  output$ui_numvar_modtab <- renderTable({
    if (is.null(input$sel_res_numvar1)) {
      return(NULL)
    }
    if (length(input$sel_res_numvar1)==0) {
      return(NULL)
    }
    df_m <- extractManipData(obj$sdcObj)
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
  }, include.rownames=FALSE)
  output$ui_numvar_origtab <- renderTable({
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
  }, include.rownames=FALSE)
  output$ui_numvar_cor <- renderUI({
    if (is.null(input$sel_res_numvar1)) {
      return(NULL)
    }
    if (length(input$sel_res_numvar1)==0) {
      return(NULL)
    }
    v_o <- get_origData()[[input$sel_res_numvar1]]
    v_m <- extractManipData(obj$sdcObj)[[input$sel_res_numvar1]]
    vv <- round(cor(v_o, v_m), digits=3)
    txt <- paste("Correlation between original and modified variable:", code(vv))
    htmlTemplate("tpl_one_col.html",inp=h5(HTML(txt)))
  })
  if (!has_numkeyvars()) {
    return(uiOutput("nonumkey_continous_results"))
  }
  list(
    htmlTemplate("tpl_two_col.html",inp1=uiOutput("ui_numvar_numres"), inp2=uiOutput("ui_catvar_numres")),
    uiOutput("ui_numvar_cor"),
    htmlTemplate("tpl_one_col.html",inp=h5("Original Values")),
    htmlTemplate("tpl_one_col.html",inp=tableOutput("ui_numvar_origtab")),
    htmlTemplate("tpl_one_col.html",inp=h5("Modified Data")),
    htmlTemplate("tpl_one_col.html",inp=tableOutput("ui_numvar_modtab")))
})
