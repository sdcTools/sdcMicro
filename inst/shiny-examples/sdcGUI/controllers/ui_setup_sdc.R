# Message, if inputdata were loaded but no categorical variables are available
# at least one categorical variable is required to setup the sdc-problem
output$noCatVars <- renderUI({
  txt <- "Please go back to Tab 'Microdata' and create at least one factor variable!"
  fluidRow(
    column(12, h4("No categorical variables available!", align="center")),
    column(12, p(txt, align="center"))
  )
})

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
      column(12, p("The dataset consists of", code(x$dims[1]),"records and",code(x$dims[2]),"variables."), align="center"),
      column(12, list("Categorical key variables:", lapply(x$keyVars, function(x) {code(x)})), align="center"))

    if (length(x$numVars)>0) {
      out <- list(out, fluidRow(
        column(12, list("Numerical key variables", lapply(x$numVars, function(x) {code(x)})), align="center")))
    }
    if (length(x$weightVar)>0) {
      out <- list(out, fluidRow(
        column(12, list("Sampling weights:", lapply(x$weightVar, function(x) {code(x)})), align="center")))
    }
    if (length(x$strataVar)>0) {
      out <- list(out, fluidRow(
        column(12, list("Stratification variable", lapply(x$strataVar, function(x) {code(x)})), align="center")))
    }
    if (length(x$householdId)>0) {
      out <- list(out, fluidRow(
        column(12, list("Household/cluster variable:", lapply(x$householdId, function(x) {code(x)})), align="center")))
    }
    if (length(x$delVars)>0) {
      out <- list(out, fluidRow(
        column(12, list("Deleted variables", lapply(x$delVars, function(x) {code(x)})), align="center")))
    }
    gV <- x$ghostVars
    if (length(gV)>0) {
      out <- list(out, fluidRow(column(12, list(tags$br(),"Selected ghost varibles (linked variables)"), align="center")))
      for (i in 1:length(gV)) {
        out <- list(out, fluidRow(
          column(12, list("Variable(s)", lapply(gV[[i]][[2]], function(x) {
            code(x)}),"are linked to key variable", code(gV[[i]][[1]])), align="center")))
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
      In parenthesis, the same statistics are shown for the unmodified data. Note: NA (missings) are counted as separate categories!"
    dt <- data.table(
      "keyVar"=x$keyVars,
      "Number of categories"=paste(x$categories$orig, x$categories$mod),
      "Mean size"=paste(x$meansize$orig, x$meansize$mod),
      "Size of smallest"=paste(x$minsize$orig, x$minsize$mod))
    out <- fluidRow(
      column(12, h4("Information on categorical key variables"), align="center"),
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
      "k-anonimity"=paste0(c(2,3,5),"-anonymity"),
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
  output$show_info_catrisk <- renderUI({
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(invisible(NULL))
    }
    x <- print(curObj, type="risk", docat=FALSE)
    reident <- x[[1]]$reident
    riskyobs <- x[[1]]$riskyObs
    out <- fluidRow(
      column(12, h4("Risk-measures for categorical variables"), align="center"),
      column(12, p("We expect",code(reident$mod),"(",code(paste0(reident$mod_p,"%")),") re-identifications in the population. In the original
        data, we expected to have",code(reident$orig),"(",code(paste0(reident$orig_p,"%")),") re-identifications."), align="center"),
      column(12, p("Currently there are",code(riskyobs$mod),"observations that have a higher risk that the main part of the data. In the
        original data this number was",code(riskyobs$orig),"."), align="center"))
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
      column(12, h4("Information on risk for numerical key variables"), align="center"),
      column(12, p("The disclosure risk is currently between",code("0%"),"and",code(paste0(x$risk_up,"%")),".
      In the original data the risk is assumed to be between",code("0%"),"and",code("100%"),"."), align="center"),
      column(12, h4("Information loss"), align="center"),
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
      column(12, h4("Information on local suppression"), align="center"),
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
    anon_methods <- unique(anonPerformed())
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
    column(12, uiOutput("show_info_catrisk")),
    column(12, uiOutput("show_info_kanon")),
    column(12, uiOutput("show_info_pram")),
    column(12, uiOutput("show_info_localsuppression")),
    column(12, uiOutput("show_info_comp_numvars")),
    column(12, uiOutput("show_info_risk")),
    column(12, uiOutput("anonMethods"))))
  out
})

## explore current variables
output$ui_sdcObj_explorevars <- renderUI({
  output$ui_selanonvar1 <- renderUI({
    selectInput("view_selanonvar1", choices=allVars(), label=h5("Choose a variable"), multiple=FALSE, selected=obj$inp_sel_anonvar1, width="100%")
  })
  # This is required so that usual changes of the dropdown-select are also reflected in the reactive variable obj$inp_sel_anonvar1
  observeEvent(input$ui_selanonvar1, {
    obj$inp_sel_anonvar1 <- input$ui_selanonvar1
  })
  output$ui_selanonvar2 <- renderUI({
    selectInput("view_selanonvar2", choices=c("none", allVars()), label=h5("Choose a second variable (optional)"), multiple=FALSE, width="100%")
  })

  observeEvent(input$view_selanonvar1, {
    vv <- allVars()
    ii <- which(input$view_selanonvar1==vv)
    if (length(ii)>0) {
      vv <- c("none",vv[-c(ii)])
      updateSelectInput(session, inputId="view_selanonvar2", choices=vv, selected=input$view_selanonvar2)
    }
  })
  observeEvent(input$view_selanonvar2, {
    vv <- allVars()
    ii <- which(input$view_selanonvar2==vv)
    if (length(ii)>0) {
      vv <- vv[-c(ii)]
      updateSelectInput(session, inputId="view_selanonvar1", choices=vv, selected=input$view_selanonvar1)
    }
  })

  output$view_summary_anon <- renderUI({
    req(input$view_selanonvar1, input$view_selanonvar1)
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(NULL)
    }
    inputdata <- extractManipData(curObj, randomizeRecords="no")
    v1 <- input$view_selanonvar1
    v2 <- input$view_selanonvar2
    if (is.null(v1)) {
      return(NULL)
    }

    if (!is.null(v2) && v2!="none") {
      df <- data.frame(inputdata[[v1]], inputdata[[v2]])
      colnames(df) <- c(v1, v2)
      cl1 <- class(df[[1]]) %in% c("factor", "character")
      cl2 <- class(df[[2]]) %in% c("factor", "character")
    } else {
      df <- data.frame(inputdata[[v1]])
      colnames(df) <- v1
      cl1 <- class(df[[1]]) %in% c("factor", "character")
    }
    if (!is.null(v2) && v2=="none") {
      if (cl1) {
        res <- list(tab=summaryfn(inputdata[[v1]]))
        colnames(res$tab) <- c(v1, "Frequency", "Percentage")
      } else {
        res <- list(tab=as.data.frame(t(summaryfn(inputdata[[v1]]))))
      }
    } else {
      # 2 factors
      if (cl1 & cl2) {
        res <- list(tab=as.data.frame.table(addmargins(table(df[[1]], df[[2]], useNA="always"))), var=c(v1, v2))
        colnames(res$tab) <- c(res$var, "Frequency")
        res$tab$Frequency <- as.integer(res$tab$Frequency)
        res$tab$Percentage <- formatC(100*(res$tab$Frequency/nrow(df)), format="f", digits=2)
      } else if (cl1 & !cl2) {
        res <- tapply(df[[2]], df[[1]], summaryfn)
        res <- do.call("rbind", res)
        bb <- data.frame(f=rownames(res))
        colnames(bb) <- v1
        res <- cbind(bb, res)
        rownames(res) <- NULL
        res <- list(tab=res)
      } else if (!cl1 & cl2) {
        res <- tapply(df[[1]], df[[2]], summaryfn)
        res <- do.call("rbind", res)
        bb <- data.frame(f=rownames(res))
        colnames(bb) <- v2
        res <- cbind(bb, res)
        rownames(res) <- NULL
        res <- list(tab=res)
      } else {
        # two numeric variables
        tab1 <- as.data.frame(t(summaryfn(df[[1]])))
        tab2 <- as.data.frame(t(summaryfn(df[[2]])))
        vcor <- round(cor(df[[1]], df[[2]], use="pairwise.complete.obs"),3)
        res <- list(vars=c(v1,v2),tab1=tab1, tab2=tab2, vcor=vcor)
      }
    }

    out <- NULL
    if (is.null(res$tab1)) {
      out <- list(out, fluidRow(column(12, renderTable(res$tab, include.rownames=FALSE), align="center")))
    } else {
      out <- list(out, fluidRow(
        column(12, h5(HTML(paste("Correlation between",code(res$vars[1]),"and",code(res$vars[2]),":",code(res$vcor))), align="center")),
        column(12, h5(HTML(paste("Summary of Variable",code(res$vars[1]))), align="center")),
        column(12, renderTable(res$tab1, include.rownames=FALSE), align="center"),
        column(12, h5(HTML(paste("Summary of Variable",code(res$vars[2]))), align="center")),
        column(12, renderTable(res$tab2, include.rownames=FALSE), align="center")))
    }

    nainfo <- data.frame(variable=c(v1, v2))
    nainfo$nr_na <- as.integer(unlist(lapply(df, function(x) { sum(is.na(x)) })))
    nainfo$perc_na <- formatC(100*(nainfo$nr_na/nrow(df)), format="f", digits=2)
    out <- list(out,
      fluidRow(column(12, "Variable",code(nainfo$variable[1]),"has",code(nainfo$nr_na[1]),"(",code(paste0(nainfo$perc_na[1],"%")),") missing values.", align="center")))
    if (nrow(nainfo)==2 & nainfo$variable[2]!="none") {
      out <- list(out,
        fluidRow(column(12, "Variable",code(nainfo$variable[2]),"has",code(nainfo$nr_na[2]),"(",code(paste0(nainfo$perc_na[2],"%")),") missing values.", align="center")))
    }
    out
  })
  output$view_plot_anon <- renderPlot({
    req(input$view_selanonvar1, input$view_selanonvar2)
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(NULL)
    }
    inputdata <- extractManipData(curObj, randomizeRecords="no")
    vv1 <- inputdata[[input$view_selanonvar1]]
    if (input$view_selanonvar2=="none") {
      if (is.factor(vv1) | is.character(vv1)) {
        tt <- table(vv1, useNA="always")
        names(tt)[length(tt)] <- "NA"
        barplot(tt, col="#DADFE1")
      } else {
        hist(vv1, main=NULL, xlab=input$view_selanonvar1, col="#DADFE1")
      }
    } else {
      vv2 <- inputdata[[input$view_selanonvar2]]
      cl1 <- class(vv1) %in% c("factor", "character")
      cl2 <- class(vv2) %in% c("factor", "character")
      df <- data.frame(vv1, vv2)
      vars <-  c(input$view_selanonvar1, input$view_selanonvar2)
      colnames(df) <- vars
      if (cl1 & cl2) {
        n <- length(unique(df[[vars[1]]]))
        cols <- colorRampPalette(c("#DADFE1", "#1E824C"), alpha=TRUE)(n)
        mosaicplot(as.formula(paste("~",paste(vars,collapse="+"),sep="")),data=df,main="", color=cols)
      } else if (cl1 & !cl2) {
        boxplot(df[[2]]~df[[1]], xlab=vars[1], ylab=vars[2], col="#DADFE1")
      } else if (!cl1 & cl2) {
        boxplot(df[[1]]~df[[2]], xlab=vars[2], ylab=vars[1], col="#DADFE1")
      } else {
        plot(df, xlab=vars[1], ylab=vars[2])
      }
    }
  })

  if (!is.null(lastError())) {
    return(fluidRow(
      column(12, h4("The following Error has occured!", align="center")),
      column(12, code(lastError()))))
  }

  out <- fluidRow(column(12, h4("Analyze existing variables in current sdc Problem", align="center")))
  out <- list(out, fluidRow(
    column(6, uiOutput("ui_selanonvar1")),
    column(6, uiOutput("ui_selanonvar2"))))

  out <- list(out, fluidRow(
    column(12, plotOutput("view_plot_anon", height="500px"))
  ))
  out <- list(out, uiOutput("view_summary_anon"))
  out
})

## add Ghost-Vars
output$ui_sdcObj_addghostvars <- renderUI({
  btn_ghosts <- myActionButton("btn_addGhostVars",label=("add 'Ghost'-variables"), "primary", css.class="btn-xs")
  res <- possGhostVars()
  gv1 <- selectInput("sel_gv1", label=h5("Select key-Variable"), choices=res$kv, selected=input$sel_gv1, width="100%")
  gv2 <- selectInput("sel_gv2", label=h5("Select ghost-variables"), choices=res$gv, selected=input$sel_gv2,multiple=TRUE, width="100%")

  if (length(res$gv) == 0) {
    return(fluidRow(column(12,
      h4("No variables are available that could be used as",code("ghost-variables"),".", align="center"))))
  }
  out <- fluidRow(column(12, h4("Add 'Ghost-Vars' to the existing Problem", align="center")))
  out <- list(out, fluidRow(
    column(6, gv1, align="center"),
    column(6, gv2, align="center")
  ))
  if (!is.null(input$sel_gv2) && length(input$sel_gv2)>0) {
    out <- list(out, fluidRow(column(12, btn_ghosts, align="center")))
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
      choices=c("none",allVars()), multiple=FALSE, width="100%")
  })
  output$randid_btn <- renderUI({
    req(input$txt_randid_newid)
    if (input$txt_randid_newid=="") {
      return(NULL)
    }
    if (input$txt_randid_newid %in% allVarsP()) {
      return(NULL)
    }
    myActionButton("btn_addRandID", label=("Add new ID-variable"), "primary")
  })
  out <- fluidRow(column(12, h4("Add a new random ID variable to the the existing Problem", align="center")))
  out <- list(out, fluidRow(
    column(6, uiOutput("randid_newid"), align="center"),
    column(6, uiOutput("randid_withinvar"), align="center")
  ))
  out <- list(out, fluidRow(
    column(12, uiOutput("randid_btn"), align="center")
  ))
  out
})

sdcData <- reactive({
  inputdata <- inputdata()
  if (is.null(inputdata)) {
    return(NULL)
  }
  vars <- allVars()

  vv <- obj$setupval_inc
  df <- data.frame(
    "Variable Name"=vars,
    Type=dataTypes(),
    Key=shinyInput(radioButtons, length(vars), paste0("setup_key_",vv,"_"), choices=c("No", "Cat.", "Cont."), inline=TRUE),
    Weight=shinyInput(checkboxInput, length(vars), paste0("setup_weight_",vv,"_"), value=FALSE, width="20px"),
    Remove=shinyInput(checkboxInput, length(vars), paste0("setup_remove_",vv,"_"), value=FALSE, width="20px"),
    "Cluster ID"=shinyInput(checkboxInput, length(vars), paste0("setup_cluster_",vv,"_"), value=FALSE, width="20px")
  )
  df$nrCodes <- sapply(inputdata, function(x) { length(unique(x))} )
  df$nrNA <- sapply(inputdata, function(x) { sum(is.na(x))} )
  rownames(df) <- NULL
  df
})

output$setupTable <- DT::renderDataTable({
  sdcData()
}, server=FALSE, escape=FALSE, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
options = list(
  searching=FALSE, paging=FALSE, ordering=FALSE, bInfo=FALSE, autoWidth=FALSE,
  columnDefs=list(list(width='400px', targets = c(2))),
  preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
  drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
))

# show the setup-button or an error-message
output$setupbtn <- renderUI({
  vv <- obj$setupval_inc
  if (is.null(input[[paste0("setup_key_",vv,"_1")]])) {
    return(NULL)
  }
  n <- length(allVars())
  types <- dataTypes()
  useAsKeys <- shinyValue(paste0("setup_key_",vv,"_"), n)
  useAsWeight <- shinyValue(paste0("setup_weight_",vv,"_"), n)
  useAsClusterID <- shinyValue(paste0("setup_cluster_",vv,"_"), n)
  deleteVariable <- shinyValue(paste0("setup_remove_",vv,"_"), n)

  ## key-variables
  # no categorical key variable
  if (sum(useAsKeys=="Cat.")==0) {
    return(myErrBtn("tmp", label="Error: No categorical key variables selected"))
  }
  # some selected categorical key-variables are numeric or character
  ii <- which(useAsKeys=="Cat." & types%in%c("numeric","character"))
  if (length(ii)>0) {
    txt <- paste0(" Categorical key variables have to be of type ",dQuote("factor"), " or type ", dQuote("integer"),". Please go back to the Microdata tab to convert the variables to the appropriate type.")
    return(modalDialog(list(p(txt)), title="Error", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
  }

  # some selected numerical key-variables are factor or character
  ii <- which(useAsKeys=="Cont." & types%in%c("factor","character"))
  if (length(ii)>0) {
    txt <- paste0("Continuous key variables have to be of type ",dQuote("numeric")," or type ",dQuote("integer"),". Please go back to the Microdata tab to convert the variables to the appropriate type.")
    return(modalDialog(list(p(txt)), title="Error", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
  }

  ## weight-variables
  ii <- which(useAsWeight==TRUE)
  # more than one weight-variable
  if (length(ii)>1) {
    return(myErrBtn("tmp", label="Error: More than one weight variable selected"))
  }
  if (length(ii)==1) {
    # weights can't be any-key variables
    if (useAsKeys[ii]!="No") {
      return(myErrBtn("tmp", label="Error: Weight variable cannot be selected as (numerical) key-variable"))
    }
    # weight-variables must be numeric
    if (!types[ii] %in% c("numeric","integer")) {
      txt <- paste0("The weight variable has to be of type ",dQuote("numeric")," or type ", dQuote("integer"),". ")
      txt <- paste0(txt, "Please go back to the Microdata tab to convert the variables to the appropriate type or change the variable type in the dataset and reload the data.")
      return(modalDialog(list(p(txt)), title="Error", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
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
      return(myErrBtn("tmp", label="Error: Cluster-id variable cannot be selected as key variable"))
    }
  }

  ## delete-variables must not be selected as anything else
  ii <- which(deleteVariable==TRUE)
  if (length(ii)>0) {
    if (any(useAsKeys[ii] %in% c("Cat.","Cont."))) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be key variables"))
    }
    if (any(useAsWeight[ii]==TRUE)) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be the weight variable"))
    }
    if (any(useAsClusterID[ii]==TRUE)) {
      return(myErrBtn("tmp", label="Error: Variables that should be deleted must not be the cluster-id variable"))
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
  inputdata <- inputdata()
  input$sel_infov
  isolate({
    if (is.null(input$sel_infov)) {
      colnames(inputdata)[1]
    } else {
      input$sel_infov
    }
  })
})
output$ui_sdcObj_info <- renderUI({
  # dependency on select-variable
  input$sel_infov
  inputdata <- inputdata()
  isolate({
    inp <- inputdata[[cur_infovar()]]
    if (is.integer(inp) & length(unique(inp))<=10) {
      inp <- as.factor(inp)
    }

    out <- NULL
    if (is.factor(inp)) {
      out <- list(out, fluidRow(
        column(12, renderPlot(plot(inp, main=NULL)), align="center")))
      ui_nrLevs <- p("Number of levels including NA:", code(length(table(inp, useNA="always"))))
    } else {
      out <- list(out, fluidRow(
        column(12, renderPlot(hist(inp, main=NULL)), align="center")))
      ui_nrLevs <- p("Number of unique values including NA:", code(length(table(inp, useNA="always"))))
    }
    out <- list(out, fluidRow(
      column(12, ui_nrLevs, align="center"),
      column(12, renderPrint(summary(inp)))))
    out
  })
})

output$sel_sdc_infovar <- renderUI({
  selectInput("sel_infov", label=h4("Select variable to show information"), choices=allVars(), width="100%")
})

output$ui_sdcObj_create <- renderUI({
  out <- fluidRow(
    column(8, div(style='padding-right: 15px;height: 550px; overflow-y: scroll',uiOutput("ui_sdcObj_create1")), uiOutput("setup_moreparams"), uiOutput("setupbtn")),
    column(4, uiOutput("sel_sdc_infovar"), uiOutput("ui_sdcObj_info"), align="center")
  )
  out
})
