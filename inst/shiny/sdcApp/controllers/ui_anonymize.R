choices_anon_menu <- reactive({
  c_manage <- c(
    "Show Summary",
    "Explore variables",
    "Add 'Ghost'-Variables",
    "Create new IDs")
  df1 <- data.frame(name=c_manage, group=1, header=NA, stringsAsFactors=FALSE)
  df1$header[1] <- "View/Analyze existing sdcProblem"

  c_cat <- c(
    "Recoding",
    "k-Anonymity",
    "Postrandomization (simple)",
    "Postrandomization (expert)",
    "Supress values with high risks")
  df2 <- data.frame(name=c_cat, group=2, header=NA, stringsAsFactors=FALSE)
  df2$header[1] <- "Anonymize categorical variables"

  c_num <- c(
    "Top/bottom coding",
    "Microaggregation",
    "Adding Noise",
    "Rank Swapping")
  if (length(get_numVars())==0) {
    c_num <- c_num[1]
  }
  df3 <- data.frame(name=c_num, group=3, header=NA, stringsAsFactors=FALSE)
  df3$header[1] <- "Anonymize numerical variables"

  df <- rbind(df1, df2, df3)
  df
})

## left sidebar
output$ui_anonymize_sidebar_left <- renderUI({
  output$ui_sdcObj_reset <- renderUI({
    if (is.null(sdcObj())) {
      return(NULL)
    }
    btn_reset <- bsButton("btn_reset_sdc1", label=("Delete SDC problem"), style="warning", size="extra-small", block="TRUE")
    fluidRow(
      column(12, h4("Reset the Problem"), align="center"),
      column(12, btn_reset))
  })
  output$ui_sel_anon_btns <- renderUI({

    df_choices <- choices_anon_menu()
    out <- NULL
    for (i in 1:nrow(df_choices)) {
      curid <- paste0("btn_sel_anon_",i)
      if (!is.na(df_choices$header[i])) {
        out <- list(out, fluidRow(column(12, h4(df_choices$header[i]), align="center")))
      }
      if (obj$cur_selection_anon==curid) {
        style <- "primary"
      } else {
        style <- "default"
      }
      out <- list(out, fluidRow(
        column(12, bsButton(curid, label=df_choices$name[i], block=TRUE, size="extra-small", style=style), tags$br())))
    }
    eval(parse(text=genObserver_menus(pat="btn_sel_anon_", n=1:nrow(df_choices), updateVal="cur_selection_anon")))
    out
  })
  out <- list(uiOutput("ui_sdcObj_reset"), uiOutput("ui_sel_anon_btns"))
  out
})

# center column
output$ui_main_anon <- renderUI({
  val <- obj$cur_selection_anon
  if (val=="btn_sel_anon_2") {
    return(uiOutput("ui_sdcObj_explorevars"))
  }
  if (val=="btn_sel_anon_3") {
    return(uiOutput("ui_sdcObj_addghostvars"))
  }
  if (val=="btn_sel_anon_4") {
    return(uiOutput("ui_sdcObj_randIds"))
  }

  ## categorical methods
  if (val=="btn_sel_anon_5") {
    return(uiOutput("ui_recode"))
  }
  if (val=="btn_sel_anon_6") {
    return(uiOutput("ui_kAnon"))
  }
  if (val=="btn_sel_anon_7") {
    return(uiOutput("ui_pram_simple"))
  }
  if (val=="btn_sel_anon_8") {
    return(uiOutput("ui_pram_expert"))
  }
  if (val=="btn_sel_anon_9") {
    return(uiOutput("ui_supp_threshold"))
  }
  ## numerical methods
  if (val=="btn_sel_anon_10") {
    return(uiOutput("ui_topbotcoding_num"))
  }
  if (val=="btn_sel_anon_11") {
    return(uiOutput("ui_microaggregation"))
  }
  if (val=="btn_sel_anon_12") {
    return(uiOutput("ui_noise"))
  }
  if (val=="btn_sel_anon_13") {
    return(uiOutput("ui_rankswap"))
  }
  return(invisible(NULL))
})


output$ui_anonymize_noproblem <- renderUI({
  return(list(
    noInputData(uri="ui_anonymize"),
    fluidRow(column(12, tags$br(), p("or go back to tab 'Undo' and upload a previously saved problem instance"), align="center")),
    fluidRow(column(12, myActionButton("nodata_anonymize_uploadproblem", label="Upload a previously saved problem", btn.style="primary"), align="center"))
  ))
})
output$ui_anonymize_summary <- renderUI({
  return(fluidRow(
    column(2, uiOutput("ui_anonymize_sidebar_left")),
    column(10, uiOutput("ui_sdcObj_summary"))))
})
output$ui_anonymize_withsidebar <- renderUI({
  return(fluidRow(
    column(2, uiOutput("ui_anonymize_sidebar_left")),
    column(7, uiOutput("ui_main_anon")),
    column(3, isolate(uiOutput("sb_info_anonymize")))
  ))
})


output$ui_anonymize <- renderUI({
  if (is.null(obj$inputdata)) {
    return(uiOutput("ui_anonymize_noproblem"))
  }
  if (is.null(sdcObj())) {
    return(uiOutput("ui_sdcObj_create"))
  }
  ## sdcMicroObj-based
  if(obj$cur_selection_anon=="btn_sel_anon_1") {
    return(uiOutput("ui_anonymize_summary"))
  } else {
    return(uiOutput("ui_anonymize_withsidebar"))
  }
  return(invisible(NULL))
})
