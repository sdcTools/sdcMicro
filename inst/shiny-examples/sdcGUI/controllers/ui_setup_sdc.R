# Message, if inputdata were loaded but no categorical variables are available
# at least one categorical variable is required to setup the sdc-problem
output$noCatVars <- renderUI({
  txt <- "Please go back to Tab 'Microdata' and create at least one factor-variable!"
  fluidRow(
    column(12, h4("No categorical variables available!", align="center")),
    column(12, p(txt, align="center"))
  )
})

## reset buttons

## show summary
output$ui_sdcObj_summary <- renderUI({
  output$show_info_general <- renderUI({
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(invisible(NULL))
    }
    x <- print(curObj, type="general", docat=FALSE)
    out <- fluidRow(
      column(12, h4("Important variables and information"), align="center"),
      column(12, p("The dataset consists of", code(x$dims[1]),"and",code(x$dims[2]),"variables."), align="center"),
      column(12, list("Categorical key variables:", lapply(x$keyVars, function(x) {code(x)})), align="center"))

    if (length(x$numVars)>0) {
      out <- list(out, fluidRow(
        column(12, list("Numerical key variables", lapply(x$numVars, function(x) {code(x)})), align="center")))
    }
    if (length(x$weightVar)>0) {
      out <- list(out, fluidRow(
        column(12, list("Sampling-Weights:", lapply(x$weightVar, function(x) {code(x)})), align="center")))
    }
    if (length(x$strataVar)>0) {
      out <- list(out, fluidRow(
        column(12, list("Numerical key variables", lapply(x$strataVar, function(x) {code(x)})), align="center")))
    }
    if (length(x$delVars)>0) {
      out <- list(out, fluidRow(
        column(12, list("Deleted Variables", lapply(x$delVars, function(x) {code(x)})), align="center")))
    }
    gV <- x$ghostVars
    if (length(gV)>0) {
      out <- list(out, fluidRow(column(12, list(tags$br(),"ghostVariable(s) exist!"), align="center")))
      for (i in 1:length(gV)) {
        out <- list(out, fluidRow(
          column(12, list("Variable(s)", lapply(gV[[i]][[2]], function(x) {
            code(x)}),"are linked to key-variable", code(gV[[i]][[1]])), align="center")))
      }
    }
    out <- list(out, fluidRow(
      column(12, h4("Computation time"), align="center"),
      column(12, p("The current computation time was ~",code(comptime()),".", align="center"))))
    out
  })
  output$show_info_recodes <- renderUI({
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(invisible(NULL))
    }
    x <- print(curObj, type="recode", docat=FALSE)
    if (is.null(x)) {
      return(invisible(NULL))
    }
    txt <-"Reported is the number, mean size and size of the smallest category for recoded variables.
      In parenthesis, the same statistics are shown for the unmodified data. Note: NA (missings) are counted as seperate categories!"
    dt <- data.table(
      "keyVar"=x$keyVars,
      "Number of categories"=paste(x$categories$orig, x$categories$mod),
      "Mean size"=paste(x$meansize$orig, x$meansize$mod),
      "Size of smallest"=paste(x$minsize$orig, x$minsize$mod))
    out <- fluidRow(
      column(12, h4("Information on categorical key-variables"), align="center"),
      column(12, p(txt), align="center"),
      column(12, renderTable(dt), align="center"))
    out
  })
  output$show_info_kanon <- renderUI({
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(invisible(NULL))
    }
    x <- print(curObj, type="kAnon", docat=FALSE)
    txt <- "Below the number of observations violating k-Anonymity is shown for the original data and
    the current dataset"
    dt <- data.table(
      "Typ"=paste0(c(2,3,5),"-anonymity"),
      "Current data"=c(paste0(x[["2anon"]]$mod," (",x[["2anon"]]$mod_p,"%)"),
          paste0(x[["3anon"]]$mod," (",x[["3anon"]]$mod_p,"%)"),
          paste0(x[["5anon"]]$mod," (",x[["5anon"]]$mod_p,"%)")),
      "Original data"=c(paste0(x[["2anon"]]$orig," (",x[["2anon"]]$orig_p,"%)"),
          paste0(x[["3anon"]]$orig," (",x[["3anon"]]$orig_p,"%)"),
          paste0(x[["5anon"]]$orig," (",x[["5anon"]]$orig_p,"%)")))
    out <- fluidRow(
      column(12, h4("Information on k-Anonymity"), align="center"),
      column(12, p(txt), align="center"),
      column(12, renderTable(dt), align="center"))
    out
  })
  output$show_info_risk <- renderUI({
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
      column(12, p("The disclosure risk is currently between",code("0%"),"and",code(paste0(x$risk_up,"%")),".
      In the original data the risk is assumed to be between",code("0%"),"and",code("100%"),"."), align="center"),
      column(12, h4("Information-Loss"), align="center"),
      column(12, p("Measure",strong("IL1"),"is",code(x$il1),"and the",strong("differences of eigenvalues"),"are",code(paste0(x$diff_eigen,"%")),"."), align="center")
    )
    out
  })
  output$show_info_localsuppression <- renderUI({
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(invisible(NULL))
    }
    x <- print(curObj, type="ls", docat=FALSE)
    if (is.null(x)) {
      return(NULL)
    }
    if(!is.na(x$threshold)) {
      meth <- "localSupp()"
      txt <- NULL
    } else {
      meth <- "kAnon()"
      if (length(x$strataVars)>0) {
        txt <- "Note: k-anonymity was applied per strata!"
      } else {
        txt <- NULL
      }
    }

    dt <- data.table(keyVar=x$supps$KeyVar)
    dt[,v1:=paste0(x$supps[[2]]," (",x$supps[[3]],"%)")]
    dt[,v2:=paste0(x$suppsT[[2]]," (",x$suppsT[[3]],"%)")]
    setnames(dt, c("Key Variable", paste("Additional Supps due to last run of",meth), "Total Suppressions"))
    out <- list(fluidRow(
      column(12, h4("Information on Local Suppression"), align="center"),
      column(12, p(txt), align="center"),
      column(12, renderTable(dt), align="center")))
  })
  output$show_info_pram <- renderUI({
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(invisible(NULL))
    }
    x <- print(curObj, type="pram", docat=FALSE)
    if (is.null(x)) {
      return(NULL)
    }
    out <- fluidRow(column(12, h4("Postrandomization"), align="center"))
    dt <- x$pram_summary
    for (i in 1:nrow(dt)) {
      out <- list(out, fluidRow(
        column(12, p("Variable",code(dt$variable[i])), align="center"),
        column(12, tags$i("final transition matrix"), align="center"),
        column(12, uiOutput(paste0("transmat_pram_",i)), align="center")))
    }
    out <- list(out, fluidRow(
      column(12, p("Summary of changed observations due to postrandomization"), align="center"),
      column(12, renderTable(dt), align="center")))
    out
  })
  output$show_info_comp_numvars <- renderUI({
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(invisible(NULL))
    }
    x <- print(curObj, type="comp_numvars", docat=FALSE)
    if (is.null(x) || length(x$results)==0) {
      return(NULL)
    }
    dt <- rbindlist(x$results)
    dt <- cbind(data.table(Variable=rep(x$numVars, each=2)), dt)
    out <- fluidRow(column(12, h4("Compare numVars"), align="center"))
    out <- list(out, fluidRow(
      column(12, renderTable(dt), align="center")
    ))
    out
  })
  output$anonMethods <- renderUI({
    anon_methods <- anonPerformed()
    if (is.null(anon_methods)) {
      return(invisible(NULL))
    } else {
      out <- fluidRow(column(12, h4("Anonymization steps"),align="center"))
      for (i in 1:length(anon_methods)) {
        out <- list(out, fluidRow(column(12, code(anon_methods[i]),align="center")))
      }
    }
    out
  })

  cur_warning <- lastWarning()
  out <- NULL
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
    column(12, uiOutput("show_info_general")),
    column(12, uiOutput("show_info_recodes")),
    column(12, uiOutput("show_info_kanon")),
    column(12, uiOutput("show_info_pram")),
    column(12, uiOutput("show_info_localsuppression")),
    column(12, uiOutput("show_info_comp_numvars")),
    column(12, uiOutput("show_info_risk")),
    column(12, uiOutput("anonMethods"))))
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

output$setupTable <- DT::renderDataTable({
  if (is.null(obj$inputdata)) {
    return(NULL)
  }
  vars <- allVars()
  df <- data.frame(
    "Variable Name"=vars,
    Type=dataTypes(),
    Key=shinyInput(radioButtons, length(vars), 'setup_key_', choices=c("No", "Cat.","Cont.")),
    Pram=shinyInput(checkboxInput, length(vars), 'setup_pram_', value=FALSE, width="20px"),
    Weight=shinyInput(checkboxInput, length(vars), 'setup_weight_', value=FALSE, width="20px"),
    "Cluster ID"=shinyInput(checkboxInput, length(vars), 'setup_cluster_', value=FALSE, width="20px"),
    Strata=shinyInput(checkboxInput, length(vars), 'setup_strata_', value=FALSE, width="20px"),
    Remove=shinyInput(checkboxInput, length(vars), 'setup_remove_', value=FALSE, width="20px")
  )
  df$nrCodes <- sapply(obj$inputdata, function(x) { length(unique(x))} )
  df$nrNA <- sapply(obj$inputdata, function(x) { sum(is.na(x))} )
  rownames(df) <- NULL
  df
}, server = FALSE, escape = FALSE, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed', options = list(
  searching=FALSE, paging=FALSE, ordering=FALSE, bInfo = FALSE,
  autoWidth=FALSE,
  columnDefs=list(list(width='150px', targets = c(0:2))),
  columnDefs=list(list(width='20px', targets=c(3:9))),
  preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
))

# show the setup-button or an error-message
output$setupbtn <- renderUI({
  if (is.null(input$setup_key_1)) {
    return(NULL)
  }
  n <- length(allVars())
  types <- dataTypes()
  useAsKeys <- shinyValue("setup_key_", n)
  useAsPram <- shinyValue("setup_pram_", n)
  useAsWeight <- shinyValue("setup_weight_", n)
  useAsClusterID <- shinyValue("setup_cluster_", n)
  deleteVariable <- shinyValue("setup_remove_", n)
  useAsStrata <- shinyValue("setup_strata_", n)

  ## key-variables
  # no categorical key variable
  if (sum(useAsKeys=="Cat.")==0) {
    return(myErrBtn("tmp", label="Error: No categorical key-variables selected"))
  }
  # some selected categorical key-variables are numeric or character
  ii <- which(useAsKeys=="Cat." & types%in%c("numeric","character"))
  if (length(ii)>0) {
    return(myErrBtn("tmp", label="Error: Selected categorical key-variables are of type 'numeric' or 'character'"))
  }
  # some selected numerical key-variables are factor or character
  ii <- which(useAsKeys=="Cont." & types%in%c("factor","character"))
  if (length(ii)>0) {
    return(myErrBtn("tmp", label="Error: Selected continous key-variables are of type 'factor' or 'character'"))
  }

  ## pram
  ii <- which(useAsPram)
  if (length(ii)>0) {
    # selected pram vars must not be key-vars
    if (any(useAsKeys[ii] %in% c("Cat.","Cont."))) {
      return(myErrBtn("tmp", label="Error: Selected pram-variables are also key-variables"))
    }
    if (any(useAsWeight[ii] == TRUE)) {
      return(myErrBtn("tmp", label="Error: Selected pram-variable is also the weight variable"))
    }
    if (any(useAsClusterID[ii] == TRUE)) {
      return(myErrBtn("tmp", label="Error: Selected pram-variable is also the cluster-id variable"))
    }
  }

  ## weight-variables
  ii <- which(useAsWeight==TRUE)
  # more than one weight-variable
  if (length(ii)>1) {
    return(myErrBtn("tmp", label="Error: More than one weight-variable selected"))
  }
  if (length(ii)==1) {
    # weights can't be any-key variables
    if (useAsKeys[ii]!="No") {
      return(myErrBtn("tmp", label="Error: Weight variable cannot be selected as (numerical) key-variable"))
    }
    # weight-variables must be numeric
    if (!types[ii] %in% c("numeric","integer")) {
      return(myErrBtn("tmp", label="Error: Weight variable must be of type 'numeric' or 'integer'"))
    }
  }

  ## cluster-ids
  ii <- which(useAsClusterID==TRUE)
  # more than one cluster-ids
  if (length(ii)>1) {
    return(myErrBtn("tmp", label="Error: More than one cluster-id variable selected"))
  }
  if (length(ii)==1) {
    # cluster-ids can't be any-key variables
    if (useAsKeys[ii]!="No") {
      return(myErrBtn("tmp", label="Error: Cluster-id variable cannot be selected as key-variable"))
    }
  }

  ## delete-variables must not be selected as anything else
  ii <- which(deleteVariable==TRUE)
  if (length(ii)>0) {
    if (any(useAsKeys[ii] %in% c("Cat.","Cont."))) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be key-variables"))
    }
    if (any(useAsPram[ii]==TRUE)) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be pram-variables"))
    }
    if (any(useAsWeight[ii]==TRUE)) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be the weight-variable"))
    }
    if (any(useAsClusterID[ii]==TRUE)) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be the cluster-id variable"))
    }
    if (any(useAsStrata[ii]==TRUE)) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be strata-variables"))
    }
  }
  btn <- myActionButton("btn_setup_sdc",label=("Setup SDC Problem"), "primary")
  fluidRow(column(12, div(btn, align="center")))
})

# show additional parameters
output$setup_moreparams <- renderUI({
  sl_alpha <- sliderInput("sl_alpha", label=h5("Parameter 'alpha'"), value=1, min=0, max=1, step=0.01, width="90%")
  help_alpha <- helpText("The higher alpha, the more keys containing missing values will contribute to the calculation of 'fk' and 'Fk'")
  sl_seed <- sliderInput("sl_seed", label=h5("Parameter 'seed'"), value=0, min=-250, max=250, step=1, round=FALSE, width="90%")
  help_seed <- helpText("Select an initial (integer) value for the random seed generator")
  out <- list(
    fluidRow(column(6, sl_alpha, align="center"), column(6, sl_seed, align="center")),
    fluidRow(column(6, help_alpha, align="center"), column(6, help_seed, align="center")))
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
    fluidRow(column(12, DT::dataTableOutput("setupTable", height="100%"))))
  out
})

# initialize with default value!
cur_infovar <- reactive({
  if (is.null(input$sel_infov)) {
    colnames(obj$inputdata)[1]
  } else {
    input$sel_infov
  }
})
output$ui_sdcObj_info <- renderUI({
  # dependency on select-variable
  input$sel_infov
  isolate({
    inp <- obj$inputdata[[cur_infovar()]]
    if (is.integer(inp) & length(unique(inp))<=10) {
      inp <- as.factor(inp)
    }
    ui_nrLevs <- p("Number of levels including NA:",code(length(table(inp,useNA="always"))))

    out <- NULL
    if (is.factor(inp)) {
      out <- list(out, fluidRow(
        column(12, renderPlot(plot(inp, main=NULL)), align="center")))
    } else {
      out <- list(out, fluidRow(
        column(12, renderPlot(hist(inp, main=NULL)), align="center")))
    }
    out <- list(out, fluidRow(
      column(12, ui_nrLevs, align="center"),
      column(12, renderPrint(summary(inp)))))
    out
  })
})

output$ui_sdcObj_create <- renderUI({
  sel_infov <- selectInput("sel_infov", label=h4("Select Variable to show Information"), choices=allVars(), selected=input$sel_infov, width="100%")
  out <- fluidRow(
    column(8, div(style='padding-right : 15px;height: 350px; overflow-y: scroll',uiOutput("ui_sdcObj_create1")), uiOutput("setup_moreparams"), uiOutput("setupbtn")),
    column(4, sel_infov, uiOutput("ui_sdcObj_info"), align="center")
  )
  out
})
