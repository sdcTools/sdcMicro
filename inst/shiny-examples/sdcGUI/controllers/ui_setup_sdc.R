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
  comptime <- reactive({
    z <- as.difftime(obj$comptime, units="secs")

    if (obj$comptime <60) {
      return(paste(round(as.numeric(z, units="secs"), digits=2),"seconds"))
    } else {
      return(paste(round(as.numeric(z, units="mins"), digits=2),"minutes"))
    }
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
  out <- list(out, htmlTemplate("tpl_one_col.html", inp=p("The current computation time was ~",code(comptime()),".")))
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

## create the sdcMicroObj problem instance
output$setupbtn <- renderUI({
  fluidRow(column(6, offset=6, myActionButton("btn_setup_sdc",label=("Setup SDC Problem"), "primary")))
})
output$setup_moreparams <- renderUI({
  sl_ranseed <- sliderInput("sl_ranseed", label=h5("Random Seed"), value=input$sl_ranseed, min=-10000, max=10000, step=1, width="100%")
  help_ranseed <- helpText("Set an initial start-value for the random-seed generator.")
  rb_randomize <- radioButtons("rb_setup_randomizeorder", label=h5("Randomize Order of Observations"), choices=c("No"=FALSE,"Yes"=TRUE),
    width="100%", selected=input$rb_setup_randomizeorder, inline=FALSE)
  help_randomize <- helpText("If you want to randomize the order of the observations, please specify",tags$i("yes"),".")
  if (is.null(input$sl_alpha)) {
    val_alpha <- 1
  } else {
    val_alpha <- input$sl_alpha
  }
  sl_alpha <- sliderInput("sl_alpha", label=h5("Parameter 'alpha'"), value=val_alpha, min=0, max=1, step=0.01, width="100%")
  help_alpha <- helpText("The higher alpha, the more keys containing missing values will contribute to the calculation of 'fk' and 'Fk'")
  out <- list(
    fluidRow(column(4, sl_ranseed), column(4, rb_randomize), column(4, sl_alpha)),
    fluidRow(column(4, help_ranseed), column(4, help_randomize), column(4, help_alpha)))
  out
})

output$ui_sdcObj_create1 <- renderUI({
  input$btn_reset_sdc # dependency so that variable-types will get updated!
  out <- NULL
  if (!is.null(obj$last_error)) {
    out <- list(out, fluidRow(column(12, verbatimTextOutput("ui_lasterror"))))
  }

  isolate({
    out <- list(out, fluidRow(
      column(2, strong("Variable Name")),
      column(1, strong("Type")),
      column(3, strong("Assign Key")),
      column(1, strong("Pram")),
      column(1, strong("Weight")),
      column(1, strong("Cluster-Id")),
      column(1, strong("Stratification")),
      column(1, strong("Delete")),
      column(1,"")
    ))
    vars <- allVars()
    type <- dataTypes()
    for (i in 1:length(vars)) {
      ch <- c("no","categorical", "numeric")
      if (type[i] == "numeric" ) {
        ch <- c("no","numeric")
      }
      if (type[i] %in% c("factor","character")) {
        ch <- c("no","categorical")
      }

      lab_kv <- paste0("rb_keyVars_",i)
      val_kv <- input[[lab_kv]]
      if (is.null(val_kv)) {
        val_kv <- ch[1]
      }

      # pram
      lab_p <- paste0("cb_setup_pram",i)
      val_p <- input[[lab_p]]
      if (is.null(val_p)) {
        val_p <- FALSE
      }
      if (type[i] %in% c("integer","numeric")) {
        cb_p <- checkboxInput(lab_p, label=NULL, value=val_p)
      } else {
        cb_p <- NULL
      }


      # weight-variable
      lab_w <- paste0("cb_setup_weight",i)
      val_w <- input[[lab_w]]
      if (is.null(val_w)) {
        val_w <- FALSE
      }
      if (type[i] %in% c("integer","numeric")) {
        cb_w <- checkboxInput(lab_w, label=NULL, value=val_w)
      } else {
        cb_w <- NULL
      }
      # household-id
      lab_h <- paste0("cb_setup_household",i)
      val_h <- input[[lab_h]]
      if (is.null(val_h)) {
        val_h <- FALSE
      }

      # strata-variable
      lab_s <- paste0("cb_setup_strata",i)
      val_s <- input[[lab_s]]
      if (is.null(val_s)) {
        val_s <- FALSE
      }

      # delete variables
      lab_d <- paste0("cb_setup_delete",i)
      val_d <- input[[lab_d]]
      if (is.null(val_d)) {
        val_d <- FALSE
      }

      out <- list(out, fluidRow(
        column(2, code(vars[i])),
        column(1, type[i]),
        column(3, radioButtons(paste0("rb_keyVars_",i), label=NULL, inline=TRUE, choices=ch, selected=val_kv)),
        column(1, cb_p),
        column(1, cb_w),
        column(1, checkboxInput(lab_h, label=NULL, value=val_h)),
        column(1, checkboxInput(lab_s, label=NULL, value=val_s)),
        column(1, checkboxInput(lab_d, label=NULL, value=val_d)),
        column(1, "")
      ))
    }
  })
  out
})


output$ui_sdcObj_info <- renderUI({
  output$ui_setup_plot_info <- renderPlot({
    if (is.null(input$sel_infov)) {
      return(NULL)
    }
    inp <- obj$inputdata[[input$sel_infov]]
    if (is.integer(inp) & length(unique(inp))<=10) {
      inp <- as.factor(inp)
    }
    plot(inp, main=NULL)
  })
  output$ui_setup_summary <- renderPrint({
    if (is.null(input$sel_infov)) {
      return(NULL)
    }
    inp <- obj$inputdata[[input$sel_infov]]
    if (is.integer(inp) & length(unique(inp))<=10) {
      inp <- as.factor(inp)
    }
    summary(inp)
  })

  sel_infov <- selectInput("sel_infov", label=NULL, choices=allVars(), selected=input$sel_infov, width="100%")
  out <- list(
    fluidRow(column(12, h4("Select Variable to show Information", align="center"), sel_infov)),
    fluidRow(column(12, h4("Plot", align="center"))),
    fluidRow(column(12, plotOutput("ui_setup_plot_info"))),
    fluidRow(column(12, h4("Summary", align="center"))),
    fluidRow(column(12, verbatimTextOutput("ui_setup_summary")))
  )
  out
})

output$ui_sdcObj_create <- renderUI({
  fluidRow(
    column(8, uiOutput("ui_sdcObj_create1"), uiOutput("setup_moreparams"), uiOutput("setupbtn")),
    column(4, uiOutput("ui_sdcObj_info"))
  )
})
