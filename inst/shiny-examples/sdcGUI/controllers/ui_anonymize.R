output$ui_anonymize <- renderUI({
  output$ui_sel_anonymize <- renderUI({

    if (length(get_numVars())>0) {
      choices <- c(
        "View/Analyse existing sdcProblem"="manage_sdcProb",
        "Anonymize categorical variables"="cat_anon",
        "Anonymize numerical variables"="cat_num")
    } else {
      choices <- c(
        "View/Analyse existing sdcProblem"="manage_sdcProb",
        "Anonymize categorical variables"="cat_anon")
    }

    if (is.null(input$sel_anonymize)) {
      sel <- choices[1]
    } else {
      sel <- input$sel_anonymize
    }
    selectInput("sel_anonymize", label=h5("What do you want to do?"),
      choices=choices,selected=sel, width="100%")
  })

  output$ui_sel_sdcresults <- renderUI({
    if (is.null(input$sel_sdcresults)) {
      sel <- res_choices_anon()[1]
    } else {
      sel <- input$sel_sdcresults
    }
    selectInput("sel_sdcresults", label=h5("Choose a Method"),choices=res_choices_anon(),selected=sel, width="100%")
  })

  if ( is.null(obj$inputdata) ) {
    return(noInputData())
  }
  possVars <- sdcVars()
  if (length(setdiff(possVars$kv, "")) == 0) {
    return(uiOutput("noCatVars"))
  }

  if (is.null(obj$sdcObj)) {
    return(uiOutput("ui_sdcObj_create"))
  } else {
    out <- htmlTemplate("tpl_one_col.html",inp=h2("Anonymization"))
    sel1 <- selectInput("sel_anonymize", label=h5("What do you want to do?"),
      choices=c(
        "View/Analyse existing sdcProblem"="manage_sdcProb",
        "Anonymize categorical variables"="cat_anon",
        "Anonymize numerical variables"="cat_num"),
      selected=input$rb_anonymize, width="100%")

    out <- list(out, htmlTemplate("tpl_two_col.html",
      inp1=uiOutput("ui_sel_anonymize"), inp2=uiOutput("ui_sel_sdcresults")))

    if (!is.null(input$sel_sdcresults)) {
      ## sdcMicroObj-based
      if (input$sel_sdcresults=="sdcObj_summary") {
        out <- list(out, uiOutput("ui_sdcObj_summary"))
      }
      if (input$sel_sdcresults=="sdcObj_addghostvars") {
        out <- list(out, uiOutput("ui_sdcObj_addghostvars"))
      }
      if (input$sel_sdcresults=="sdcObj_reset") {
        out <- list(out, uiOutput("ui_sdcObj_reset"))
      }

      ## categorical methods
      if (input$sel_sdcresults=="pram") {
        out <- list(out, uiOutput("ui_pram"))
      }
      if (input$sel_sdcresults=="recode") {
        out <- list(out, uiOutput("ui_recode"))
      }
      if (input$sel_sdcresults=="kanon") {
        out <- list(out, uiOutput("ui_kAnon"))
      }
      if (input$sel_sdcresults=="supp_threshold") {
        out <- list(out, uiOutput("ui_supp_threshold"))
      }
      ## numerical methods
      if (input$sel_sdcresults=="noise") {
        out <- list(out, uiOutput("ui_noise"))
      }
      if (input$sel_sdcresults=="microaggregation") {
        out <- list(out, uiOutput("ui_microaggregation"))
      }
      if (input$sel_sdcresults=="shuffling") {
        out <- list(out, uiOutput("ui_shuffling"))
      }
    }
  }
  return(out)
})
