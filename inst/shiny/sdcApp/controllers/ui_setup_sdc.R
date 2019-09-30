# Message, if inputdata were loaded but no categorical variables are available
# at least one categorical variable is required to setup the sdc-problem
output$noCatVars <- renderUI({
  txt <- "Go back to the Microdata tab and convert at least one variable to type factor."
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
      column(12, h3("Summary of dataset and variable selection"), class="wb-header"),
      column(12, p("The loaded dataset consists of", code(x$dims[1]),"records and",code(x$dims[2]),"variables."), class="wb-header-hint"),
      column(12, list("Categorical key variable(s):", lapply(x$keyVars, function(x) {code(x)})) ))

    if (length(x$numVars)>0) {
      out <- list(out, fluidRow(
        column(12, list("Numerical key variable(s):", lapply(x$numVars, function(x) {code(x)})) )))
    }
    if (length(x$weightVar)>0) {
      out <- list(out, fluidRow(
        column(12, list("Sampling weight:", lapply(x$weightVar, function(x) {code(x)})) )))
    }
    # cannot be selected when creating the sdc problem instance
    #if (length(x$strataVar)>0) {
    #  out <- list(out, fluidRow(
    #    column(12, list("Stratification variable:", lapply(x$strataVar, function(x) {code(x)})), align="center")))
    #}
    if (length(x$householdId)>0) {
      out <- list(out, fluidRow(
        column(12, list("Hierarchical identifier:", lapply(x$householdId, function(x) {code(x)})) )))
    }
    if (length(x$delVars)>0) {
      out <- list(out, fluidRow(
        column(12, list("Deleted variable(s):", lapply(x$delVars, function(x) {code(x)})) )))
    }
    gV <- x$ghostVars
    if (length(gV)>0) {
      out <- list(out, fluidRow(column(12, tags$br(), list("Linked variables:") )))
      for (i in 1:length(gV)) {
        out <- list(out, fluidRow(
          column(12, list("Variable(s)", lapply(gV[[i]][[2]], function(x) {
            code(x)}),"are linked to key variable", code(gV[[i]][[1]])) )))
      }
    }
    out <- list(out, fluidRow(
      column(12, h4("Computation time") ),
      column(12, p("The current computation time was ~",code(comptime()),"." ))))
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
    txt <-"Reported is the number of levels, average frequency of each level and frequency of the smallest level (with frequency >0) for categorical key variables.
      In parentheses, the same statistics are shown for the original data. Note that NA (missing) is counted as a separate category."
    dt <- data.table(
      "Variable name"=x$keyVars,
      "Number of levels"=paste(x$categories$orig, x$categories$mod),
      "Average frequency"=paste(x$meansize$orig, x$meansize$mod),
      "Frequency of smallest level (>0)"=paste(x$minsize$orig, x$minsize$mod))
    out <- fluidRow(
      column(12, h4("Information on categorical key variables") ),
      column(12, p(txt) ),
      column(12, renderTable(dt), class="wn-info-table"))
    out
  })
  output$show_info_kanon <- renderUI({
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(invisible(NULL))
    }
    x <- print(curObj, type="kAnon", docat=FALSE)
    txt <- "Below the number of observations violating k-anonymity is shown for the original data and the modified dataset"
    dt <- data.table(
      "k-anonymity"=paste0(c(2,3,5),"-anonymity"),
      "Modified data"=c(paste0(x[["2anon"]]$mod," (",x[["2anon"]]$mod_p,"%)"),
          paste0(x[["3anon"]]$mod," (",x[["3anon"]]$mod_p,"%)"),
          paste0(x[["5anon"]]$mod," (",x[["5anon"]]$mod_p,"%)")),
      "Original data"=c(paste0(x[["2anon"]]$orig," (",x[["2anon"]]$orig_p,"%)"),
          paste0(x[["3anon"]]$orig," (",x[["3anon"]]$orig_p,"%)"),
          paste0(x[["5anon"]]$orig," (",x[["5anon"]]$orig_p,"%)")))
    out <- fluidRow(
      column(12, h4("Information on k-anonymity") ),
      column(12, p(txt) ),
      column(12, renderTable(dt), class="wn-info-table"))
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
    txt_risk <- "Observations with a higher risk than the main part of the data are defined as observations with an individual risk higher than the median "
    txt_risk <- paste0(txt_risk, "plus twice the median absolute deviation of the individual risk of all records in the dataset. Only individuals with an individual risk higher than 0.1 are considered.")
    out <- fluidRow(
      column(12, h4("Risk measures for categorical key variables") ),
      column(12, p("We expect",code(reident$mod),"(",code(paste0(reident$mod_p,"%")),") re-identifications in the population, as compared to",
                   code(reident$orig),"(",code(paste0(reident$orig_p,"%")),") re-identifications in the original data.") ),
      column(12, p(code(riskyobs$mod)," observations have a higher risk than the risk in the main part of the data, as compared to ",
                   code(riskyobs$orig)," observations in the original data.", tipify(icon("info-circle"), title=txt_risk, placement="top")) ))
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
      column(12, h4("Risk measures for numerical key variables") ),
      column(12, p("The disclosure risk is currently between",code("0%"),"and",code(paste0(x$risk_up,"%")),",
      as compared to between",code("0%"),"and",code("100%"),"in the original data.") ),
      column(12, h4("Information loss") ),
      column(12, p("Measure",strong("IL1s"),"is",code(x$il1),"and the",strong("differences of eigenvalues"),"are",code(paste0(x$diff_eigen,"%")),".") )
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
      txt <- txt2 <- NULL
    } else {
      meth <- "kAnon()"
      txt <- "Below the number of supressions (values set to a missing value (NA)) due to the last run of the local suppression algorithm."
      txt <- paste0(txt, " The table also displays the number of missing values (NA) per variable before applying the local suppresion algorithm as well as the ")
      txt <- paste0(txt, "the total number of missing values in each variable after applying local suppression (sum of intial missings and suppressions).")

      txt2 <- NULL
      if (length(x$strataVars)>0) {
        txt2 <- "Note: k-anonymity was applied per strata!"
      } else {
        txt2 <- NULL
      }
    }

    dt <- data.table(keyVar=x$supps$KeyVar)
    dt[,v1:=paste0(x$supps[[2]]," (",x$supps[[3]],"%)")]
    dt[,v2:=paste0(x$suppsT[[2]] - x$supps[[2]]," (",prettyF(as.numeric(100*((x$suppsT[[2]] - x$supps[[2]])/nrow(curObj@origData))), digits=3),"%)")]
    dt[,v3:=paste0(x$suppsT[[2]]," (",x$suppsT[[3]],"%)")]

    setnames(dt, c("Key variable", "Number of suppressions",
                   "Total missing values (NA) before applying local suppression", "Total missing values (NA) after applying local supression"))
    out <- list(fluidRow(
      column(12, h4("Information on local suppression") ),
      column(12, p(txt)),
      column(12, p(txt2)),
      column(12, renderTable(dt), class="wn-info-table")))
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
    out <- fluidRow(column(12, h4("PRAM") ))
    dt <- x$pram_summary
    for (i in 1:nrow(dt)) {
      out <- list(out, fluidRow(
        column(12, p("Variable",code(dt$variable[i])) ),
        column(12, tags$i("transition matrix")),
        column(12, uiOutput(paste0("transmat_pram_",i)), class="wn-info-table")))
    }
    out <- list(out, fluidRow(
      column(12, p("Summary of changed observations due to PRAM") ),
      column(12, renderTable(dt), class="wn-info-table")))
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
    dt <- rbindlist(x$results, fill=TRUE)
    if (length(grep("NA's", colnames(dt)))>0) {
      dt[is.na(`NA's`),`NA's`:="0"]
    }
    dt <- cbind(data.table(Variable=rep(x$numVars, each=2)), dt)
    out <- fluidRow(column(12, h4("Compare numerical key variables")))
    out <- list(out, fluidRow(
      column(12, renderTable(dt), class="wn-info-table")
    ))
    out
  })
  output$anonMethods <- renderUI({
    anon_methods <- unique(anonPerformed())
    out <- fluidRow(column(12, h4("Anonymization steps") ))
    if (is.null(anon_methods)) {
      out <- list(out, fluidRow(column(12, code("No methods have been applied"))))
      return(out)
    } else {
      out <- fluidRow(column(12, h4("Anonymization steps") ))
      for (i in 1:length(anon_methods)) {
        out <- list(out, fluidRow(column(12, code(anon_methods[i]))))
      }
    }
    out
  })

  cur_warning <- lastWarning()
  out <- NULL
  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Application of the last method resulted in the following error!")),
      column(12, verbatimTextOutput("ui_lasterror"))
      , class = "wb-error-toast"))
  }
  if (!is.null(lastWarning())) {
    out <- list(out, fluidRow(
      column(12, inp=h4("Application of the last method resulted in the following warning!")),
      column(12, inp=verbatimTextOutput("ui_lastwarning"))
      , class = "wb-warning-toast"))
  }
  out <- list(out, fluidRow(
    column(12, uiOutput("show_info_general")),
    column(12, uiOutput("show_info_recodes")),
    column(12, uiOutput("show_info_catrisk")),
    column(12, uiOutput("show_info_kanon"))))
  if(!is.null(sdcObj()) & !is.null(print(sdcObj(), type="pram", docat=FALSE))){
    out <- list(out, fluidRow(
      column(12, uiOutput("show_info_pram"))))
  }
  if(!is.null(sdcObj()) & !is.null(print(sdcObj(), type="ls", docat=FALSE))){
    out <- list(out, fluidRow(
      column(12, uiOutput("show_info_localsuppression"))))
  }
  if(!is.null(sdcObj()) & !is.null(print(sdcObj(), type="comp_numvars", docat=FALSE)) & !(length(print(sdcObj(), type="comp_numvars", docat=FALSE)$results)==0)){
    out <- list(out, fluidRow(
      column(12, uiOutput("show_info_comp_numvars"))))
  }
  out <- list(out, fluidRow(
    column(12, uiOutput("show_info_risk")),
    column(12, uiOutput("anonMethods"))))
  out
})

## explore current variables
output$ui_sdcObj_explorevars_header <- renderUI({
  out <- fluidRow(
    column(12, h3("Explore variables in modified data"), offset = 0, class = "wb-header"),
    column(12, p("Here you can view tabulations, summary statistics and graphic representations of variables and pairs of variables to explore the modified data."), offset = 0, class = "wb-header-hint")
  )
  out
})
output$ui_sdcObj_explorevars <- renderUI({
  output$ui_selanonvar1 <- renderUI({
    selectInput("view_selanonvar1", choices=allVars(), label=p("Choose a variable"), multiple=FALSE, selected=obj$inp_sel_anonvar1, width="100%")
  })
  # This is required so that usual changes of the dropdown-select are also reflected in the reactive variable obj$inp_sel_anonvar1
  observeEvent(input$ui_selanonvar1, {
    obj$inp_sel_anonvar1 <- input$ui_selanonvar1
  })
  output$ui_selanonvar2 <- renderUI({
    vv <- setdiff(allVars(), input$view_selanonvar1)
    selectInput("view_selanonvar2", choices=c("none", vv), label=p("Choose a second variable (optional)"), multiple=FALSE, width="100%")
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
    if (input$view_selanonvar2!="none") {
      ii <- which(input$view_selanonvar2==vv)
      if (length(ii)>0) {
        vv <- vv[-c(ii)]
      }
    }
    updateSelectInput(session, inputId="view_selanonvar1", choices=vv, selected=input$view_selanonvar1)
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
        tabdf <- addmargins(table(df[[1]], df[[2]], useNA = "always"))
        colnames(tabdf)[is.na(colnames(tabdf))] <- "NA"
        rownames(tabdf)[is.na(rownames(tabdf))] <- "NA"
        tabdfp <- prop.table(tabdf)
        tabdfc <- as.data.frame(matrix(paste0(tabdf, paste0(" (", formatC(100 * tabdfp, format = "f", digits = 2),"%)")), nrow = dim(tabdf)[1], ncol = dim(tabdf)[2]))
        colnames(tabdfc) <- colnames(tabdf)
        rownames(tabdfc) <- rownames(tabdf)
        res <- list(tab = as.data.frame.matrix(tabdfc), var = c(v1, v2))
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
    if (!is.null(v2) && v2 == "none") { # univariate
      out <- list(out, fluidRow(column(12, renderTable(res$tab, include.rownames=FALSE), class="wn-info-table")))
    } else { # bivariate
      if (cl1 & cl2){ # both factor
        out <- list(out, fluidRow(
          column(12, h5(HTML(paste("Cross-tabulation of", code(res$var[1]), "and", code(res$var[2]))))),
          column(12, renderTable(res$tab, include.rownames=TRUE), class="wn-info-table wn-row-title")))
      } else if ((cl1 & !cl2) | (!cl1 & cl2)){ # one factor, one numeric
        out <- list(out, fluidRow(column(12, renderTable(res$tab, include.rownames=FALSE), class="wn-info-table")))
      } else { # both numeric
        out <- list(out, fluidRow(
          column(12, h5(HTML(paste("Correlation between",code(res$vars[1]),"and",code(res$vars[2]),":",code(res$vcor))))),
          column(12, h5(HTML(paste("Summary of variable",code(res$vars[1]))))),
          column(12, renderTable(res$tab1, include.rownames=FALSE), class="wn-info-table"),
          column(12, h5(HTML(paste("Summary of variable",code(res$vars[2]))))),
          column(12, renderTable(res$tab2, include.rownames=FALSE), class="wn-info-table")))      }
    }

    nainfo         <- data.frame(variable=c(v1, v2))
    nainfo$nr_na   <- as.integer(unlist(lapply(df, function(x) { sum(is.na(x)) })))
    nainfo$perc_na <- formatC(100*(nainfo$nr_na/nrow(df)), format="f", digits=2)
    out <- list(out,
      fluidRow(column(12, "Variable",code(nainfo$variable[1]),"has",code(nainfo$nr_na[1]),"(",code(paste0(nainfo$perc_na[1],"%")),") missing values.")))
    if (nrow(nainfo)==2 & nainfo$variable[2]!="none") {
      out <- list(out,
      fluidRow(column(12, "Variable",code(nainfo$variable[2]),"has",code(nainfo$nr_na[2]),"(",code(paste0(nainfo$perc_na[2],"%")),") missing values.")))
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
        #barplot(tt, col="#DADFE1")
        mp <- barplot(tt, col="#DADFE1", las=1, xaxt = 'n')
        mtext(gsub(paste0("(.{", round(120/length(tt)), "})"), paste0("\\1", "\n"), names(tt)), side = 1, line = 2, at = mp)
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
      column(12, h4("The following error has occured!")),
      column(12, code(lastError()))), class = "wb-error-toast")
  }

  out <- fluidRow(
    column(6, uiOutput("ui_selanonvar1")),
    column(6, uiOutput("ui_selanonvar2")))

  out <- list(out, fluidRow(
    column(12, plotOutput("view_plot_anon", height="500px"))
  ))
  out <- list(out, uiOutput("view_summary_anon"))
  out
})

## add Ghost-Vars
output$ui_sdcObj_addghostvars_header <- renderUI({
  helptxt <- "Often datasets contain variables that are related to the key variables used for local suppression and could be used to reconstruct suppressed values. Here you can link variables to"
  helptxt <- paste(helptxt, "categorical key variables. Any suppression in the key variable will lead to a suppression in the variable(s) linked")
  helptxt <- paste(helptxt, "to that key variable. Several variables can be linked to one key variable.")
  out <-fluidRow(
    column(12, h3("Add linked variables"), offset = 0, class = "wb-header"),
    column(12, p(helptxt), offset = 0, class = "wb-header-hint")
  )
})
output$ui_sdcObj_addghostvars <- renderUI({
  output$addgv_btn <- renderUI({
    req(input$sel_gv2)
    if (length(input$sel_gv2)==0) {
      return(NULL)
    }
    btn_ghosts <- myActionButton("btn_addGhostVars", label="Add linked variables", "primary")
  })
  output$addgv_v1 <- renderUI({
    res <- possGhostVars()
    selectInput("sel_gv1", label=p("Select categorical key variable"), choices=res$kv, width="100%")
  })
  output$addgv_v2 <- renderUI({
    res <- possGhostVars()
    selectInput("sel_gv2", label=p("Select linked variable(s)"), choices=res$gv,  multiple=TRUE, width="100%")
  })

  res <- possGhostVars()
  if (length(res$gv) == 0) {
    return(fluidRow(column(12,
      h4("No variables are available that could be used as",code("linked variables"),"."))))
  }

  out <- fluidRow(
    column(6, uiOutput("addgv_v1"), align="center"),
    column(6, uiOutput("addgv_v2"), align="center")
  )
  out <- list(out, fluidRow(column(12, uiOutput("addgv_btn"), align="center")))
  out
})

## add new random ID-variable
output$ui_sdcObj_randIds_header <- renderUI({
  helptxt <- "The ID in microdata as well as the order of records can be used to reconstruct suppressed values."
  helptxt <- paste(helptxt, "Here you create a new randomized ID that can be used to replace the existing ID. To create a new household ID")
  helptxt <- paste(helptxt, "you can select the household ID as a variable for which the new ID should be the same for equal values.")
  helptxt2 <- "Note: Do not forget to remove the existing ID after exporting the data."

  out <- fluidRow(
    column(12, h3("Add a new random ID variable"), offset = 0, class = "wb-header"),
    column(12, p(helptxt), p(helptxt2), offset = 0, class = "wb-header-hint")
  )
  out
})
output$ui_sdcObj_randIds <- renderUI({
  output$randid_newid <- renderUI({
    textInput("txt_randid_newid", label=p("Specify name for the new ID variable"), width="100%")
  })
  #
  output$randid_withinvar <- renderUI({
  txt_tooltip <- "If used, the ID will be the same for equal values of the selected variable."
    selectInput("sel_randid_withinvar", label=p("Set grouping variable (optional)", tipify(icon("info-circle"), title=txt_tooltip, placement="top")),
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
    myActionButton("btn_addRandID", label=("Add new ID variable"), "primary")
  })


  out <- fluidRow(
    column(6, uiOutput("randid_newid"), align="center"),
    column(6, uiOutput("randid_withinvar"), align="center")
  )
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
    "Cluster ID"=shinyInput(checkboxInput, length(vars), paste0("setup_cluster_",vv,"_"), value=FALSE, width="20px"),
    Pram=shinyInput(checkboxInput, length(vars), paste0("setup_pram_",vv,"_"), value=FALSE, width="20px"),
    Remove=shinyInput(checkboxInput, length(vars), paste0("setup_remove_",vv,"_"), value=FALSE, width="20px")
  )
  df$nrCodes <- sapply(inputdata, function(x) { length(unique(x))} )
  df$nrNA <- sapply(inputdata, function(x) { sum(is.na(x))} )
  colnames(df) <- c("Variable name", "Type", "Key variables", "Weight", "Hierarchical<br>identifier", "PRAM", "Delete", "Number of levels", "Number of missing")
  rownames(df) <- NULL
  df
})

output$setupTable <- DT::renderDataTable({
  sdcData()
}, server=FALSE, escape=FALSE, rownames=FALSE, selection='none', style='bootstrap', class='table-condensed',
options = list(
  scrollX=TRUE, scrollY=380, searching=FALSE, paging=FALSE, ordering=FALSE, bInfo=FALSE, autoWidth=FALSE,
  # columnDefs=list(list(width='400px', targets = c(2))),
  columnDefs=list(list()),
  preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
  drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); var event = new Event("AnonymiseDrawnEvent"); document.dispatchEvent(event);} ')
))

# show the setup-button or an error-message
output$setupbtn <- renderUI({
  vv <- obj$setupval_inc
  if (is.null(input[[paste0("setup_key_",vv,"_1")]])) {
    return(NULL)
  }

  showBtn <- TRUE
  vnames <- allVars()
  n <- length(vnames)
  types <- dataTypes()
  useAsKeys <- shinyValue(paste0("setup_key_",vv,"_"), n)
  useAsPram <- shinyValue(paste0("setup_pram_",vv,"_"), n)
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
    showBtn <- FALSE
    txt <- p("Categorical key variables have to be of type",dQuote("factor"), " or type ", dQuote("integer"),".", tags$br(), tags$br(),
      tags$span(style="color:red; font-weight:bold","Change the variable selection or go back to the Microdata tab and change the variable type before making other variable selections."))
    showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice for ",dQuote(vnames[ii]))), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
  }

  # some selected numerical key-variables are factor or character
  ii <- which(useAsKeys=="Cont." & types%in%c("factor","character"))
  if (length(ii)>0) {
    showBtn <- FALSE
    txt <- p("Continuous key variables have to be of type ",dQuote("numeric")," or type ",dQuote("integer"), tags$br(), tags$br(),
      tags$span(style="color:red; font-weight:bold","Change the variable selection or go back to the Microdata tab and change the variable type before making other variable selections."))
    showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice for ",dQuote(vnames[ii]))), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
  }

  ## pram
  ii <- which(useAsPram)
  if (length(ii)>0) {
    # selected pram vars must not be key-vars
    if (any(useAsKeys[ii] %in% c("Cat.","Cont."))) {
      showBtn <- FALSE
      txt <- p("Selected pram variables are also key variables.", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold","You need to undo the pram variable selection and select only pram variables that are not selected as key variables before making other variable selections!"))
      showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[ii]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
    if (any(useAsWeight[ii] == TRUE)) {
      showBtn <- FALSE
      txt <- p("The selected PRAM variable is also selected as weight variable.", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold","Undo the PRAM variable selection and select only PRAM variables that are not selected as weight variables before making other variable selections!"))
      showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[ii]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
    if (any(useAsClusterID[ii] == TRUE)) {
      showBtn <- FALSE
      txt <- p("The selected PRAM variable is also selected as hierarchical identifier.", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold","Undo the PRAM variable selection and select only PRAM variables that are not selected as hierarchcal identifier before making other variable selections!"))
      showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[ii]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
    kk <- which(types[ii] != "factor")
    if (length(kk)>0) {
      showBtn <- FALSE
      txt <- p("Pram variables have to be of type ",dQuote("factor"),".", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold","Change the variable selection or go back to the Microdata tab and change the variable type before making other variable selections."))
      showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[ii]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
  }

  ## weight-variables
  ii <- which(useAsWeight==TRUE)
  # more than one weight-variable
  if (length(ii)>1) {
    showBtn <- FALSE
    txt <- p("More than one weight variable is selected.", tags$br(), tags$br(),
      tags$span(style="color:red; font-weight:bold","Undo the multiple weight variable selection and select only one weight variable before making other variable selections!"))
    showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[ii]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
  }
  if (length(ii)==1) {
    # weights can't be any-key variables
    if (useAsKeys[ii]!="No") {
      showBtn <- FALSE
      txt <- p("The selected weight variable is also selected as key variable.", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold","Undo the weight variable selection and select a weight variable that is not selected as key variable before making other variable selections!"))
      showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[ii]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
    # weight-variables must be numeric
    if (!types[ii] %in% c("numeric","integer")) {
      showBtn <- FALSE
      txt <- p("The weight variable has to be of type ",dQuote("numeric")," or type ", dQuote("integer"),".", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold","Change the variable selection or go back to the Microdata tab and change the variable type before making other variable selections."))
      showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (" ,dQuote(vnames[ii]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
    # weight-variable must not contain missing values (NA)
    if (any(is.na(inputdata()[[vnames[ii]]]))) {
      showBtn <- FALSE
      txt <- p("The weight variable contains", sum(is.na(inputdata()[[vnames[ii]]])),"missing value(s) (NA).", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold", "Undo the weight variable selection and select a weight variable that does not contain missing values before making other variable selections or remove the missing values in the weight variable in the dataset and reload the dataset!"))
      showModal(modalDialog(list(txt), title=strong(paste("Missing value(s) in selected weight variable (",dQuote(vnames[ii]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
  }
  ## cluster-ids
  ii <- which(useAsClusterID==TRUE)
  # more than one cluster-ids
  if (length(ii)>1) {
    showBtn <- FALSE
    txt <- p("More than one hierarchical identifier is selected.", tags$br(), tags$br(),
      tags$span(style="color:red; font-weight:bold","Undo the multiple hierarchical identifier selection and select only one hierarchcial identifier before making other variable selections."))
    showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[ii]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
  }
  if (length(ii)==1) {
    # cluster-ids can't be any-key variables
    if (useAsKeys[ii]!="No") {
      showBtn <- FALSE
      txt <- paste0("The selected hierarchical identifier is also selected as key variable.", " Undo the hierarchical identifier selection and select a hierarchical identifier that is not selected as key variable.")
      showModal(modalDialog(list(p(txt)), title=strong(paste("Invalid variable choice (",dQuote(vnames[ii]),")")), footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
  }

  ## delete-variables must not be selected as anything else
  ii <- which(deleteVariable==TRUE)
  if (length(ii)>0) {
    zz <- intersect(which(useAsKeys %in% c("Cat.","Cont.")), ii)
    if (length(zz)>0) {
      showBtn <- FALSE
      txt <- p("The variable selected to be deleted is also selected as key variable.", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold","Undo this variable selection and select only variables to be deleted that are not selected as key variable before making other variable selections."))
      showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[zz]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
    zz <- intersect(which(useAsPram), ii)
    if (length(zz)>0) {
      showBtn <- FALSE
      txt <- p("The variable selected to be deleted is also selected as PRAM variable.", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold","Undo this variable selection and select only variables to be deleted that are not selected as PRAM variable before making other variable selections."))
      showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[zz]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
    zz <- intersect(which(useAsWeight), ii)
    if (length(zz)>0) {
      showBtn <- FALSE
      txt <- p("The variable selected to be deleted is also selected as weight variable.", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold","Undo this variable selection and select only variables to be deleted that are not selected as weight variable before making other variable selections."))
      showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[zz]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
    zz <- intersect(which(useAsClusterID), ii)
    if (length(zz)>0) {
      showBtn <- FALSE
      txt <- p("Selected variable to be deleted is also selected as hierarchical identifier.", tags$br(), tags$br(),
        tags$span(style="color:red; font-weight:bold","Undo this variable selection and select only variables to be deleted that are not selected as hierarchical indentifier before making other variable selections."))
      showModal(modalDialog(list(txt), title=strong(paste("Invalid variable choice (",dQuote(vnames[zz]),")")), footer=modalButton("Continue"), size="m", easyClose=TRUE, fade=TRUE), session=session)
    }
  }
  btn <- myActionButton("btn_setup_sdc",label=("Setup SDC problem"), "primary")
  if (showBtn==TRUE) {
    return(fluidRow(column(12, div(btn, align="center"))))
  } else {
    return(invisible(NULL))
  }
})

# show additional parameters
output$setup_moreparams <- renderUI({
  txt_seed <- "The seed is used to initialize the random number generator used for probabilistic methods."
  txt_alpha <- "The parameter alpha is used to compute the frequencies of keys, which is used to compute risk"
  txt_alpha <- paste(txt_alpha, "measures for categorical key variables. Alpha is the weight with which a key that coincides based on a missing value (NA) contributes to these frequencies.")
  sl_alpha <- sliderInput("sl_alpha",
    label=p("Parameter 'alpha'", tipify(icon("info-circle"), title=txt_alpha, placement="top")),
    value=1, min=0, max=1, step=0.01, width="90%")
  sl_seed <- sliderInput("sl_seed",
    label=p("Parameter 'seed'", tipify(icon("info-circle"), title=txt_seed, placement="top")),
    value = 0, min = 0, max= 500, step = 1, round=FALSE, width="90%")
  out <- list(
    fluidRow(
      column(12, h4("Set additional parameters", class="wb-block-title")),
      column(12, sl_alpha, align="center"),
      column(12, sl_seed, align="center"))
    )
  out
})

output$ui_sdcObj_create1 <- renderUI({
  input$btn_reset_sdc # dependency so that variable-types will get updated!
  out <- NULL
  if (!is.null(obj$last_error)) {
    out <- list(out, fluidRow(column(12, verbatimTextOutput("ui_lasterror")), class = "wb-error-toast"))
  }

  txt_setup <- "Select the following variables for setting up the SDC problem instance: categorical key variables, continuous key variables (optional), variables selected for PRAM (optional), sample weight (optional), hierarchical identifier (optional), variables to be deleted (optional). Also, specify the parameter alpha and set a seed at the bottom of this page."
  txt_setup <- paste(txt_setup, tags$br(), tags$br(), "Tip - Before you start, make sure that variable types are appropriate. If not, go to the Microdata tab and convert variables to numeric or factor.")

  out <- list(out,
    fluidRow(column(12, h4("Select variables", tipify(icon("info-circle"), title=txt_setup, placement="bottom"), class="wb-block-title"), align="center")),
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
        column(12, renderPlot({
          df <- table(inp, useNA = "always")
          dn <- dimnames(df)[[1]]
          dn[length(dn)] <- "NA"
          dimnames(df)[[1]] <- dn
          mp <- barplot(df, las=1, xaxt = 'n')
          mtext(gsub(paste0("(.{", round(36/length(dn)), "})"), paste0("\\1", "\n"), dn), side = 1, line = 2, at = mp)
          #barplot(table(inp, useNA = "always"), main = NULL,
          #                              names.arg = c(head(gsub(paste0("(.{", round(36/length(table(inp, useNA = "always"))), "})"), paste0("\\1", "\n"), names(table(inp, useNA = "always"))), -1), "NA"), las = 1)
          #plot(inp, main=NULL)
        }), align="center")))
      #line = (0.5 * max(nchar(names(table(inp, useNA = "always"))) / (36/length(table(inp, useNA = "always"))))),
      ui_nrLevs <- p("Number of levels including missing (NA):", code(length(table(inp, useNA="always"))))
    } else {
      out <- list(out, fluidRow(
        column(12, renderPlot(hist(inp, main=NULL, xlab = input$sel_infov)), align="center")))
      ui_nrLevs <- p("Number of unique values including missing (NA):", code(length(table(inp, useNA="always"))))
    }

    if (is.factor(inp)) {
      tt <- as.data.frame.table(table(inp, useNA="always"))
      colnames(tt) <- c("Level", "Frequency")
      out <- list(out, fluidRow(
        column(12, ui_nrLevs, align="center"),
        column(12, renderTable(tt), class="wn-info-table")))
    } else {
      out <- list(out, fluidRow(
        column(12, ui_nrLevs, align="center"),
        column(12, renderTable(as.data.frame(t(summaryfn(inp))), include.rownames=FALSE), class="wn-info-table")))
    }
    out
  })
})

output$sel_sdc_infovar <- renderUI({
  selectInput("sel_infov", label=h4("Explore variables", class="wb-block-title"), choices=allVars(), width="100%")
})

output$ui_sdcObj_create <- renderUI({
  out <- fluidRow(
    column(width = 12, offset = 0, h3("Anonymize"), class="wb-header"),
    column(width = 12, offset = 0, p("Select variables and set parameters to create the SDC problem."), class="wb-header-hint"),
    # column(8, div(style='padding-right: 15px;height: 550px; overflow-y: scroll',uiOutput("ui_sdcObj_create1")), uiOutput("setup_moreparams"), uiOutput("setupbtn")),
    # column(8, div(style='height: 550px;',uiOutput("ui_sdcObj_create1")), uiOutput("setupbtn")),
    column(8, div(style='',uiOutput("ui_sdcObj_create1")), uiOutput("setupbtn")),
    column(4, uiOutput("setup_moreparams"), uiOutput("sel_sdc_infovar"), uiOutput("ui_sdcObj_info"), align="center")
  )
  out
})
