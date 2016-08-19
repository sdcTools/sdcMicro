# Message, if inputdata were loaded but no categorical variables are available
# at least one categorical variable is required to setup the sdc-problem
output$noCatVars <- renderUI({
  txt <- "Please go back to Tab 'Microdata' and create at least one factor-variable!"
  list(
    htmlTemplate("tpl_one_col.html", inp=h2("No categorical variables available!")),
    htmlTemplate("tpl_one_col.html", inp=strong(txt))
  )
})

## show summary
output$ui_sdcObj_summary <- renderUI({
  output$sdcInfo <- renderPrint({
    show(obj$sdcObj)
  })
  cur_warning <- lastWarning()
  out <- list(htmlTemplate("tpl_one_col.html", inp=h4("Summary of the current SDC-Problem")))
  if (!is.null(lastError())) {
    out <- list(out,
      htmlTemplate("tpl_one_col.html", inp=h4("Application of the last method resulted in the following error!")),
      htmlTemplate("tpl_one_col.html", inp=verbatimTextOutput("ui_lasterror")))
  }
  if (!is.null(lastWarning())) {
    out <- list(out,
      htmlTemplate("tpl_one_col.html", inp=h4("Application of the last method resulted in the following warning!")),
      htmlTemplate("tpl_one_col.html", inp=verbatimTextOutput("ui_lastwarning")))
  }
  out <- list(out, htmlTemplate("tpl_one_col.html", inp=verbatimTextOutput("sdcInfo")))
  out
})

## add Ghost-Vars
output$ui_sdcObj_addghostvars <- renderUI({
  btn_ghosts <- myActionButton("btn_addGhostVars",label=("add 'Ghost'-variables to existing problems"), "primary")
  res <- possGhostVars()
  gv1 <- selectInput("sel_gv1", label=h4("Select key-Variable"), choices=res$kv, selected=input$sel_gv1, width="100%")
  gv2 <- selectInput("sel_gv2", label=h4("Select ghost-variables"), choices=res$gv, selected=input$sel_gv2,multiple=TRUE, width="100%")

  if (length(res$gv) == 0) {
    return(list(htmlTemplate("tpl_one_col.html", inp=h4("No variables are available that could be used as",code("ghost-variables"),"."))))
  }
  out <- list(
    htmlTemplate("tpl_one_col.html", inp=h4("Add 'Ghost-Vars' to the existing Problem")),
    htmlTemplate("tpl_two_col.html", inp1=gv1, inp2=gv2))
  if (!is.null(input$sel_gv2) && length(input$sel_gv2)>0) {
    out <- list(out, htmlTemplate("tpl_one_col.html", inp=btn_ghosts))
  }
  out
})

## add new random ID-variable
output$ui_sdcObj_randIds <- renderUI({
  output$randid_newid <- renderUI({
    textInput("txt_randid_newid", label=h5("The desired name of your new ID-variable"), width="100%")
  })
  output$randid_withinvar <- renderUI({
    selectInput("sel_randid_withinvar", label=h5("If used, the ID will be the same for equal values of the selected variable"),
      choices=c("none",allVars()), selected=input$sel_randid_withinvar, multiple=FALSE, width="100%")
  })
  output$randid_btn <- renderUI({
    if (is.null(input$txt_randid_newid) || input$txt_randid_newid=="") {
      return(NULL)
    }
    myActionButton("btn_addRandID",label=("add a new random ID-variable"), "primary")
  })
  out <- list(
    htmlTemplate("tpl_one_col.html", inp=h4("Add a new random ID variable to the the existing Problem")),
    htmlTemplate("tpl_two_col.html", inp1=uiOutput("randid_newid"), inp2=uiOutput("randid_withinvar")),
    htmlTemplate("tpl_one_col.html", inp=uiOutput("randid_btn")))
  out
})

## reset Problem
output$ui_sdcObj_reset <- renderUI({
  btn_reset <- myActionButton("btn_reset_sdc",label=("Reset SDC Problem"), "danger")
  return(list(
    htmlTemplate("tpl_one_col.html", inp=h4("Reset the existing Problem")),
    htmlTemplate("tpl_one_col.html", inp=p("By clicking the button below, you can start from scratch!")),
    htmlTemplate("tpl_one_col.html", inp=btn_reset))
  )
})

## create problem
output$ui_sdcObj_create <- renderUI({
  possVars <- sdcVars()
  out <- list(
    htmlTemplate("tpl_one_col.html", inp=h2("Setup SDC-Problem")),
    htmlTemplate("tpl_three_col.html", inp1=h4("Choose key variables"), inp2=NULL, inp3=NULL))

  sel_kv <- selectInput("sel_kV", label=h5("Categorical key variables"), choices=possVars$kv, selected=input$sel_kV, multiple=TRUE, selectize=TRUE, width="100%")
  help_kv <- helpText("Help text for selection of categorical key-variables")
  sel_nv <- selectInput("sel_nV", label=h5("Numerical key variables"), choices=possVars$nv, selected=input$sel_nV, multiple=TRUE, selectize=TRUE, width="100%")
  help_nv <- helpText("Help text for selection of numerical key-variables")

  if (is.null(input$sl_alpha)) {
    val_alpha <- 1
  } else {
    val_alpha <- input$sl_alpha
  }

  sl_alpha <- sliderInput("sl_alpha", label=h5("Parameter 'alpha'"), value=val_alpha, min=0, max=1, step=0.01, width="100%")
  help_alpha <- helpText("The higher alpha, the more keys containing missing values will contribute to the calculation of 'fk' and 'Fk'")

  out <- list(out,
    htmlTemplate("tpl_three_col.html", inp1=sel_kv, inp2=sel_nv, inp3=sl_alpha),
    htmlTemplate("tpl_three_col.html", inp1=help_kv, inp2=help_nv, inp3=help_alpha))

  out <- list(out, htmlTemplate("tpl_three_col.html", inp1=h4("Choose auxiliary variables"), inp2=NULL, inp3=NULL))
  sel_wv <- selectInput("sel_wV", label=NULL, choices=possVars$wv, selected=input$sel_wV, multiple=FALSE, selectize=TRUE, width="100%")
  help_wv <- helpText("Help text for selection of weight variable")
  sel_cluster <- selectInput("sel_hhID", label=NULL, choices=possVars$hhid, selected=input$sel_hhID, multiple=FALSE, selectize=TRUE, width="100%")
  help_cluster <- helpText("Help for selection of cluster variable (e.g. household ID)")
  sel_strata <- selectInput("sel_strataV", label=NULL, choices=possVars$strataV, selected=input$sel_strataV, multiple=FALSE, selectize=TRUE, width="100%")
  help_strata <- helpText("If you coose a variable here, some methods (e.g. localSuppression()) will be applied independently for each value of the selected variable.")
  out <- list(out,
    htmlTemplate("tpl_three_col.html", inp1=h5("Weight variable"), inp2=h5("Cluster-ID"), inp3=h5("Strata Variable")),
    htmlTemplate("tpl_three_col.html", inp1=sel_wv, inp2=sel_cluster, inp3=sel_strata),
    htmlTemplate("tpl_three_col.html", inp1=help_wv, inp2=help_cluster, inp3=help_strata))

  # do not include some variables in the sdcObj
  sel_rmv <- selectInput("sel_removeVars", label=h5("Remove Variables"), choices=possRemoveVars(), selected=input$sel_removeVars, multiple=TRUE, selectize=TRUE, width="100%")
  sl_ranseed <- sliderInput("sl_ranseed", label=h5("Random Seed"), value=input$sl_ranseed, min=-10000, max=10000, step=1, width="100%")
  rb_randomize <- radioButtons("rb_setup_randomizeorder", label=h5("Randomize Order of Observations"), choices=c("No"=FALSE,"Yes"=TRUE),
    width="100%", selected=input$rb_setup_randomizeorder, inline=TRUE)
    out <- list(out, htmlTemplate("tpl_three_col.html", inp1=rb_randomize, inp2=sel_rmv, inp3=sl_ranseed))

  if ( length(input$sel_kV) > 0 ) {
    btn_setup <- myActionButton("btn_setup_sdc",label=("Setup SDC Problem"), "primary")
    out <- list(out, htmlTemplate("tpl_one_col.html", inp=btn_setup))
  }
  out
})
