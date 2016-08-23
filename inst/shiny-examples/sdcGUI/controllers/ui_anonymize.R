# choices for dropdown-menus in Tab 'anonymize'
choices_anon_manage <- reactive({
  choices <- c(
    "Show Summary"="sdcObj_summary",
    "Add 'Ghost'-Variables"="sdcObj_addghostvars",
    "Create new IDs"="sdcObj_randIds",
    "Reset sdcProblem"="sdcObj_reset")
  return(choices)
})
choices_anon_cat <- reactive({
  choices <- c(
    "Recoding"="recode",
    "k-Anonymity"="kanon",
    "Postrandomization (PRAM)"="pram",
    "Supress values with high risks"="supp_threshold")
  return(choices)
})
choices_anon_num <- reactive({
  choices <- c(
    "Apply Microaggregation"="microaggregation",
    "Adding Noise to the data"="noise",
    #"Shuffling the data"="shuffling",
    "Apply Rank-Swapping"="rankswap")
  return(choices)
})
res_choices_anon <- reactive({
  if (is.null(input$sel_anonymize)) {
    return(NULL)
  }
  if (!input$sel_anonymize %in% c("manage_sdcProb","cat_anon","cat_num")) {
    return(NULL)
  }
  if (input$sel_anonymize=="manage_sdcProb") {
    choices <- choices_anon_manage()
  }
  if (input$sel_anonymize=="cat_anon") {
    choices <- choices_anon_cat()
  }
  if (input$sel_anonymize=="cat_num") {
    choices <- choices_anon_num()
  }
  choices
})
choices_anonymize <- reactive({
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
  choices
})
output$ui_sel_anonymize <- renderUI({
  choices <- choices_anonymize()
  if (is.null(input$sel_anonymize)) {
    sel <- choices[1]
  } else {
    sel <- input$sel_anonymize
  }
  radioButtons("sel_anonymize", label=h5("What do you want to do?"),
    choices=choices,selected=sel, width="100%")
})
output$ui_sel_sdcresults <- renderUI({
  cc <- res_choices_anon()
  if (is.null(cc)) {
    return(NULL)
  }
  if (is.null(input$sel_sdcresults)) {
    sel <- cc[1]
  } else {
    sel <- input$sel_sdcresults
  }
  if (!is.null(sel)) {
    if (!sel %in% cc) {
      sel <- cc[1]
    }
  }
  radioButtons("sel_sdcresults", label=h5("Choose a Method"),choices=cc,selected=sel, width="100%")
})

## left sidebar
output$ui_anonymize_sidebar_left <- renderUI({
  list(uiOutput("ui_sel_anonymize"), uiOutput("ui_sel_sdcresults"))
})

## right sidebar
output$ui_anonymize_sidebar_right <- renderUI({
  list(h4("KeyVarInfo"), p("..."), h4("Risk measure"),p("..."))
})

# center column
output$ui_main_anon <- renderUI({
  if (is.null(input$sel_sdcresults)) {
    return(NULL)
  }
  ## sdcMicroObj-based
  if (input$sel_sdcresults=="sdcObj_summary") {
    return(uiOutput("ui_sdcObj_summary"))
  }
  if (input$sel_sdcresults=="sdcObj_addghostvars") {
    return(uiOutput("ui_sdcObj_addghostvars"))
  }
  if (input$sel_sdcresults=="sdcObj_randIds") {
    return(uiOutput("ui_sdcObj_randIds"))
  }
  if (input$sel_sdcresults=="sdcObj_reset") {
    return(uiOutput("ui_sdcObj_reset"))
  }
  ## categorical methods
  if (input$sel_sdcresults=="pram") {
    return(uiOutput("ui_pram"))
  }
  if (input$sel_sdcresults=="recode") {
    return(uiOutput("ui_recode"))
  }
  if (input$sel_sdcresults=="kanon") {
    return(uiOutput("ui_kAnon"))
  }
  if (input$sel_sdcresults=="supp_threshold") {
    return(uiOutput("ui_supp_threshold"))
  }
  ## numerical methods
  if (input$sel_sdcresults=="noise") {
    return(uiOutput("ui_noise"))
  }
  if (input$sel_sdcresults=="microaggregation") {
    return(uiOutput("ui_microaggregation"))
  }
  #if (input$sel_sdcresults=="shuffling") {
  #  out <- list(out, uiOutput("ui_shuffling"))
  #}
  if (input$sel_sdcresults=="rankswap") {
    return(uiOutput("ui_rankswap"))
  }
})

output$ui_anonymize <- renderUI({
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
    out <- NULL
    out <- list(out,
      fluidRow(
        column(3, uiOutput("ui_anonymize_sidebar_left")),
        column(6, uiOutput("ui_main_anon")),
        column(3, uiOutput("ui_anonymize_sidebar_right")))
    )
  }
  return(out)
})
