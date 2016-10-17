# choices for dropdown-menus in Tab 'anonymize'
choices_anon_manage <- function(){
  choices <- c(
    "Show Summary"="sdcObj_summary",
    "Add 'Ghost'-Variables"="sdcObj_addghostvars",
    "Create new IDs"="sdcObj_randIds")
  return(choices)
}
choices_anon_cat <- function() {
  choices <- c(
    "Recoding"="recode",
    "k-Anonymity"="kanon",
    "Postrandomization (PRAM)"="pram",
    "Supress values with high risks"="supp_threshold")
  return(choices)
}
choices_anon_num <- reactive({
  if (length(get_numVars()>0)) {
    return(c(
      "Top-/Bottom Coding"="topbot_num",
      "Apply Microaggregation"="microaggregation",
      "Adding Noise to the data"="noise",
      #"Shuffling the data"="shuffling",
      "Apply Rank-Swapping"="rankswap"))
  }
  if (length(numVars())>0) {
    return(c("Top-/Bottom Coding"="topbot_num"))
  }
  return(NULL)
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
  choices <- c(
    "View/Analyse existing sdcProblem"="manage_sdcProb",
    "Anonymize categorical variables"="cat_anon",
    "Anonymize numerical variables"="cat_num")
  if (length(get_numVars())>0) {
    return(choices)
  } else {
    if (length(numVars())>0) {
      return(choices)
    } else {
      return(choices[1:2])
    }
  }
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
  req(input$sel_anonymize)
  cc <- res_choices_anon()
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
output$ui_sdcObj_reset <- renderUI({
  invalidateLater(15000)
  if (obj$reset_sdc1>0) {
    # show real reset button!
    btn_reset <- myActionButton("btn_reset_sdc",label=("Really?"), "danger",  css.class="btn-xs")
  } else {
    btn_reset <- myActionButton("btn_reset_sdc1",label=("Click here to start from scratch"), "warning", css.class="btn-xs")
  }
  btn_reset
})

output$ui_reset <- renderUI({
  fluidRow(
    column(12, h5("Reset the Problem")),
    column(12, uiOutput("ui_sdcObj_reset"))
  )
})

output$ui_anonymize_sidebar_left <- renderUI({
  list(uiOutput("ui_reset"), uiOutput("ui_sel_anonymize"), uiOutput("ui_sel_sdcresults"))
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
  if (input$sel_sdcresults=="topbot_num") {
    return(uiOutput("ui_topbotcoding_num"))
  }
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
  if (is.null(obj$inputdata)) {
    return(noInputData(uri="ui_anonymize"))
  }

  if (is.null(sdcObj())) {
    return(uiOutput("ui_sdcObj_create"))
  } else {
    if(is.null(input$sel_sdcresults) || input$sel_sdcresults=="sdcObj_summary") {
      out <- list(fluidRow(
        column(2, uiOutput("ui_anonymize_sidebar_left")),
        column(10, uiOutput("ui_main_anon"))))
    } else {
      out <- list(fluidRow(
        column(2, uiOutput("ui_anonymize_sidebar_left")),
        column(7, uiOutput("ui_main_anon")),
        column(3, uiOutput("sb_info_anonymize"))))
    }
  }
  return(out)
})
