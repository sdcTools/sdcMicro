# Message, if inputdata were loaded but no categorical variables are available
# at least one categorical variable is required to setup the sdc-problem
output$noCatVars <- renderUI({
  txt <- "Please go back to Tab 'Microdata' and create at least one factor-variable!"
  fluidRow(
    column(12, h4("No categorical variables available!", align="center")),
    column(12, p(txt, align="center"))
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
  out <- fluidRow(column(12, h4("Summary of the current SDC-Problem", align="center")))
  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Application of the last method resulted in the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))))
  }
  if (!is.null(lastWarning())) {
    out <- list(out, fluidRow(
      column(12, inp=h4("Application of the last method resulted in the following warning!", align="center")),
      column(12, inp=verbatimTextOutput("ui_lastwarning"))))
  }

  out <- list(out, fluidRow(
    column(12, p("The current computation time was ~",code(comptime()),".", align="center")),
    column(12, verbatimTextOutput("sdcInfo"))
  ))
  out
})

## add Ghost-Vars
output$ui_sdcObj_addghostvars <- renderUI({
  btn_ghosts <- myActionButton("btn_addGhostVars",label=("add 'Ghost'-variables"), "primary")
  res <- possGhostVars()
  gv1 <- selectInput("sel_gv1", label=h5("Select key-Variable"), choices=res$kv, selected=input$sel_gv1, width="100%")
  gv2 <- selectInput("sel_gv2", label=h5("Select ghost-variables"), choices=res$gv, selected=input$sel_gv2,multiple=TRUE, width="100%")

  if (length(res$gv) == 0) {
    return(fluidRow(column(12,
      h4("No variables are available that could be used as",code("ghost-variables"),".", align="center"))))
  }
  out <- fluidRow(column(12, h4("Add 'Ghost-Vars' to the existing Problem", align="center")))
  out <- list(out, fluidRow(
    column(6, p(gv1, align="center")),
    column(6, p(gv2, align="center"))
  ))
  if (!is.null(input$sel_gv2) && length(input$sel_gv2)>0) {
    out <- list(out, fluidRow(column(12, p(btn_ghosts, align="center"))))
  }
  out
})

## add new random ID-variable
output$ui_sdcObj_randIds <- renderUI({
  output$randid_newid <- renderUI({
    textInput("txt_randid_newid", label=h5("Choose a suitable name of the new ID-variable"), width="100%")
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
  out <- fluidRow(column(12, h4("Add a new random ID variable to the the existing Problem", align="center")))
  out <- list(out, fluidRow(
    column(6, uiOutput("randid_newid")),
    column(6, uiOutput("randid_withinvar"))
  ))
  out <- list(out, fluidRow(
    column(12, uiOutput("randid_btn"))
  ))
  out
})

## reset Problem
output$ui_sdcObj_reset <- renderUI({
  btn_reset <- myActionButton("btn_reset_sdc",label=("Reset SDC Problem"), "danger")
  fluidRow(
    column(12, h4("Reset the existing Problem", align="center")),
    column(12, p("By clicking the button below, you can start from scratch!", align="center")),
    column(12, p(btn_reset, align="center")))
})

## create the sdcMicroObj problem instance
setup_data <- reactive({
  if (is.null(obj$inputdata)) {
    return(NULL)
  }
  df <- data.frame(
    Name=allVars(),
    Type=dataTypes(),
    useAsKey="do not use variable as key",
    useAsPram=FALSE,
    useAsWeight=FALSE,
    useAsClusterID=FALSE,
    useAsStrata=FALSE,
    deleteVariable=FALSE
  )
  rownames(df) <- NULL
  df
})

# show the interactive table
output$setupTable <- renderRHandsontable({
  inp <- setup_data()
  if (is.null(inp)) {
    return(NULL)
  }
  rhandsontable(inp, useTypes=TRUE, contextMenu=FALSE, overflow="visible") %>%
    hot_col(col="useAsKey", type = "dropdown", source = c("do not use variable as key","categorical","numeric"), halign="center") %>%
    hot_col(col="Name", readOnly=TRUE) %>%
    hot_col(col="Type", readOnly=TRUE) %>%
    hot_col(col="useAsPram", halign="htCenter") %>%
    hot_col(col="useAsWeight", halign="htCenter") %>%
    hot_col(col="useAsClusterID", halign="htCenter") %>%
    hot_col(col="useAsStrata", halign="htCenter") %>%
    hot_col(col="deleteVariable", halign="htCenter")
})

# show the setup-button or an error-message
output$setupbtn <- renderUI({
  if (is.null(input$setupTable)) {
    return(NULL)
  }
  # checks
  inp <- hot_to_r(input$setupTable)

  ## key-variables
  # no categorical key variable
  if (sum(inp$useAsKey=="categorical")==0) {
    return(myErrBtn("tmp", label="Error: No categorical key-variables selected"))
  }
  # some selected categorical key-variables are numeric or character
  ii <- which(inp$useAsKey=="categorical" & inp$Type%in%c("numeric","character"))
  if (length(ii)>0) {
    return(myErrBtn("tmp", label="Error: Selected categorical key-variables are of type 'numeric' or 'character'"))
  }
  # some selected numerical key-variables are factor or character
  ii <- which(inp$useAsKey=="numeric" & inp$Type%in%c("factor","character"))
  if (length(ii)>0) {
    return(myErrBtn("tmp", label="Error: Selected continous key-variables are of type 'factor' or 'character'"))
  }

  ## pram
  ii <- which(inp$useAsPram)
  if (length(ii)>0) {
    # selected pram vars must not be key-vars
    if (any(inp$useAsKey[ii] %in% c("categorical","numerical"))) {
      return(myErrBtn("tmp", label="Error: Selected pram-variables are also key-variables"))
    }
    if (any(inp$useAsWeight[ii] == TRUE)) {
      return(myErrBtn("tmp", label="Error: Selected pram-variable is also the weight variable"))
    }
    if (any(inp$useAsClusterID[ii] == TRUE)) {
      return(myErrBtn("tmp", label="Error: Selected pram-variable is also the cluster-id variable"))
    }
  }

  ## weight-variables
  ii <- which(inp$useAsWeight==TRUE)
  # more than one weight-variable
  if (length(ii)>1) {
    return(myErrBtn("tmp", label="Error: More than one weight-variable selected"))
  }
  if (length(ii)==1) {
    # weights can't be any-key variables
    if (inp$useAsKey[ii]!="do not use variable as key") {
      return(myErrBtn("tmp", label="Error: Weight variable cannot be selected as (numerical) key-variable"))
    }
    # weight-variables must be numeric
    if (!inp$Type[ii] %in% c("numeric","integer")) {
      return(myErrBtn("tmp", label="Error: Weight variable must be of type 'numeric' or 'integer'"))
    }
  }

  ## cluster-ids
  ii <- which(inp$useAsClusterID==TRUE)
  # more than one cluster-ids
  if (length(ii)>1) {
    return(myErrBtn("tmp", label="Error: More than one cluster-id variable selected"))
  }
  if (length(ii)==1) {
    # cluster-ids can't be any-key variables
    if (inp$useAsKey[ii]!="do not use variable as key") {
      return(myErrBtn("tmp", label="Error: Cluster-id variable cannot be selected as key-variable"))
    }
  }

  ## delete-variables must not be selected as anything else
  ii <- which(inp$deleteVariable==TRUE)
  if (length(ii)>0) {
    if (any(inp$useAsKey[ii] %in% c("categorical","numerical"))) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be key-variables"))
    }
    if (any(inp$useAsPram[ii]==TRUE)) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be pram-variables"))
    }
    if (any(inp$useAsWeight[ii]==TRUE)) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be the weight-variable"))
    }
    if (any(inp$useAsClusterID[ii]==TRUE)) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be the cluster-id variable"))
    }
    if (any(inp$useAsStrata[ii]==TRUE)) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be strata-variables"))
    }
  }
  btn <- myActionButton("btn_setup_sdc",label=("Setup SDC Problem"), "primary")
  fluidRow(column(12, div(btn, align="center")))
})

# show additional parameters
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
  out <- list(out,
    fluidRow(column(12, h4("Setup an sdc-Problem", align="center"))),
    fluidRow(column(12, rHandsontableOutput("setupTable", height="100%"))))
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
