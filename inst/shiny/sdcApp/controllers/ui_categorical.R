# UI-output for global recode
output$ui_recode_header <- renderUI({
  out <- fluidRow(
    column(12, h3("Recode categorical key variables" ), offset = 0, class = "wb-header"),
    column(12, p("To reduce risk, it is often useful to combine the levels of categorical key variables into a new, combined category.
                 You need to select a categorical key variable and then choose two or more levels, which you want to combine.
                 Once this has been done, a new label for the new category can be assigned." ),
               p("Note: If you only select only one level, you can rename the selected value." ),
                 offset = 0, class = "wb-header-hint"))
    out
})
output$ui_recode <- renderUI({
  # current factor-levels
  curRecFacVals <- reactive({
    req(input$sel_recfac)
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(NULL)
    }
    ff <- get_manipKeyVars()[[input$sel_recfac]]
    ll <- as.list(levels(ff))
    names(ll) <- paste0(ll, " (",table(ff)," obs)")
    ll
  })

  # plot of current factor
  output$plot_facRec <- renderPlot({
    if (is.null(input$sel_recfac)) {
      return(NULL)
    }
    df <- table(get_manipKeyVars()[[input$sel_recfac]], useNA="always")
    dn <- dimnames(df)[[1]]
    dn[length(dn)] <- "NA"
    dimnames(df)[[1]] <- dn
    barplot(df)
  })

  output$recfac_selfac1 <- renderUI({
    # current categorical key variables
    kv <- get_keyVars_names()
    selectInput("sel_recfac",label=p("Choose factor variable"), choices=kv, selected=input$sel_recfac, width="50%")
  })
  output$recfac_cbgr <- renderUI({
    cbgr <- selectInput("cbg_recfac",label=p("Select levels to recode/combine"), multiple=TRUE, selectize=TRUE,
      choices=curRecFacVals(), width="100%")
  })
  output$recfac_addna <- renderUI({
    req(input$cbg_recfac)
    radioButtons("rb_recfac_micro_addna", label=p("Add missing values to new factor level?"), choices=c("no", "yes"), inline=TRUE)
  })
  output$recfac_btn <- renderUI({
    req(input$cbg_recfac)
    myActionButton("btn_update_recfac",label="Recode key variable	", "primary")
  })
  output$recfac_txtval <- renderUI({
    req(input$cbg_recfac)
    txtval <- textInput("inp_newlevname_rec",label=p("Specify new label for recoded values"),
      value=paste0(input$cbg_recfac, collapse="_"), width="100%")
  })
  out <- fluidRow(
    column(12, uiOutput("recfac_selfac1"), align="center"))
  out <- list(out, fluidRow(
    column(4, uiOutput("recfac_cbgr"), align="center"),
    column(4, uiOutput("recfac_txtval"), align="center"),
    column(4, uiOutput("recfac_addna"), align="center")))
  out <- list(out, fluidRow(
    column(12, uiOutput("recfac_btn"), align="center"),
    column(12, plotOutput("plot_facRec"))))
  out
})

# UI-output for postrandomization (expert-useage)
output$ui_pram_expert_header <- renderUI({
  out <- fluidRow(column(12, h3("Postrandomization (PRAM) (expert usage)"), offset = 0, class = "wb-header"))
  out <- list(out, fluidRow(
    column(12, p("The PRAM algorithm randomly changes the values of selected variables in some records according to a custom-defined transition matrix."),
               p("The user can freely specify a transition matrix, which will be used for the post-randomization of a single variable. The requirement
                  is that all row sums of the specified matrix sum up to 100!"),
               offset = 0, class = "wb-header-hint")))
  out
})
output$ui_pram_expert <- renderUI({
  # update obj$transmat if table has been changed
  observe({
    if(!is.null(input$pram_expert_transmat)) {
      obj$transmat <- hot_to_r(input$pram_expert_transmat)
    }
  })
  # Fire event when selected pram-variable changes
  # initialize transition-matrix with 0.9 in diagonals
  # because with 1 some error with rhandsontable is available.
  observeEvent(input$sel_pramvars_expert, {
    if (!is.null(input$sel_pramvars_expert) && length(input$sel_pramvars_expert)==1) {
      v <- get_origData()[[input$sel_pramvars_expert]]
      ll <- levels(v)
      m <- diag(length(ll))
      diag(m) <- 100
      rownames(m) <- colnames(m) <- ll
      obj$transmat <- m
    }
  })
  # Transitionmatrix for PRAM
  output$pram_expert_transmat <- renderRHandsontable({
    if (is.null(input$sel_pramvars_expert) || length(input$sel_pramvars_expert)!=1) {
      return(NULL)
    }
    rn <- attr(obj$transmat, "dimnames")[[2]]
    m <- rhandsontable(obj$transmat, rowHeaders=rn) #%>% hot_context_menu(allowRowEdit=FALSE, allowColEdit=FALSE)
    m
  })
  output$pram_expert_strata <- renderUI({
    txt_tooltip <- "By default PRAM is applied on the complete dataset. To apply the algorithm within strata, select a variable for stratification. The algorithm is then applied within the strata defined by the factor levels of that variable."
    selectInput("pram_expert_strataV",
      label=p("Postrandomize within different groups (stratification)?", tipify(icon("info-circle"), title=txt_tooltip, placement="top")),
      choices=c("no stratification", poss_strataVarP()), multiple=FALSE, width="100%")
  })
  output$pram_expert_var <- renderUI({
    txt_tooltip <- "The expert mode allows specifying the transition matrix manually."
    selectInput("sel_pramvars_expert", choices=pramVars(),
      label=p("Select variable for PRAM", tipify(icon("info-circle"), title=txt_tooltip, placement="top")),
      selected=input$sel_pramvars_expert, width="100%", multiple=FALSE)
  })
  output$pram_expert_btn <- renderUI({
    req(input$pram_expert_strataV)
    if (is.null(obj$transmat)) {
      return(NULL)
    }
    if (!all(rowSums(obj$transmat)==100)) {
      return(myActionButton("btn_pram_expert_notworking", label="Error: Not all row-sums of the transition matrix equal 100", btn="danger"))
    }
    if (input$pram_expert_strataV %in% input$sel_pramvars_expert) {
      txt <- "You have selected a variable relevant for stratification that should also be pramed. This is not possible. Please remove the variable from one of the inputs."
      return(modalDialog(list(p(txt)), title="Error", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
    }
    return(myActionButton("btn_pram_expert", label="Postrandomize", btn="primary"))
  })
  output$pram_expert_warning <- renderUI({
    if (is.null(lastWarning())) {
      return(NULL)
    }
    fluidRow(
      column(12, h5("Application of the postrandomization attempt resulted in the following warning!", align="center")),
      column(12, verbatimTextOutput("ui_lastwarning"))
      , class = "wb-warning-toast")
  })
  output$pram_expert_error <- renderUI({
    if (is.null(lastError())) {
      return(NULL)
    }
    fluidRow(
      column(12, h5("Application of the postrandomization attempt resulted in the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))
      , class = "wb-error-toast")
  })
  if (length(pramVars())==0) {
    return(fluidRow(
      column(12, h4("Postrandomization of categorical variables", align="center")),
      column(12, h5("No variables have been specified for postrandomization during the initialization of the current problem or
        all possible variables already have been postrandomized!", align="center"))
    ))
  }

  out <- fluidRow(column(12, h3("Postrandomization (PRAM) (expert usage)"), align="center"))
  out <- list(uiOutput("pram_expert_error"), uiOutput("pram_expert_warning"))

  out <- list(out, fluidRow(
    column(12, p("The PRAM algorithm randomly changes the values of selected variables in some records according to a custom-defined transition matrix."), align="center")),
    column(12, p("The user can freely specify a transition matrix, which will be used for the post-randomization of a single variable. The requirement
      is that all row sums of the specified matrix sum up to 100!"), align="center"))
  out <- list(out, fluidRow(
    column(6, uiOutput("pram_expert_var"), align="center"),
    column(6, uiOutput("pram_expert_strata"), align="center")))

  txt_tooltip_mat <- "Each row specifies the probability that the given value is changed to one of the values in the top row."
  out <- list(out, fluidRow(
    column(12, p("Specify the transition matrix. Note: the entries in each rows must add up to 100.", tipify(icon("info-circle"), title=txt_tooltip_mat, placement="top")), align="center"),
    column(12, rHandsontableOutput("pram_expert_transmat", width="100%")),
    column(12, tags$br(), uiOutput("pram_expert_btn"), align="center")))
  out
})

# UI-output for postrandomization (simple-useage)
output$ui_pram_simple_header <- renderUI({
  out <- fluidRow(column(12, h3("Postrandomization (PRAM)"), offset = 0, class = "wb-header"))
  out <- list(out, fluidRow(
    column(12, p("The PRAM algorithm randomly changes the values of selected variables in some records according
                 to an invariant probability transition matrix."),
               p("The invariant probability transition matrix is set by specifying two parameters (",code("pd"),"and",code("alpha"),")."),
               offset = 0, class = "wb-header-hint")))
  out
})
output$ui_pram_simple <- renderUI({
  output$pram_simple_var <- renderUI({
    txt_tooltip <- "An invariant transition matrix is used, which guarantees that the univariate distribution of the variables in unchanged in probability."
    selectInput("sel_pramvars_simple", choices=pramVars(),
      label=p("Select variable(s) for PRAM", tipify(icon("info-circle"), title=txt_tooltip, placement="top")), width="100%", multiple=TRUE)
  })
  output$pram_simple_strata <- renderUI({
    txt_tooltip <- "By default PRAM is applied on the complete dataset. To apply the algorithm within strata, select a variable for stratification. The algorithm is then applied within the strata defined by the factor levels of that variable."
    selectInput("pram_strataV_simple",
      label=p("Postrandomize within different groups (stratification)?", tipify(icon("info-circle"), title=txt_tooltip, placement="top")),
      choices=c("no stratification", poss_strataVarP()), multiple=FALSE, width="100%")
  })
  output$pram_simple_pd <- renderUI({
    txt_tooltip <- "pd refers to the minimum diagonal values in the (internally) generated transition matrix. The higher this value is, the more likely it is that values are not changed."
    sliderInput("pram_simple_pd", min=0.01, max=1.00, step=0.01, value=0.8,
      label=p("Choose value for 'pd'", tipify(icon("info-circle"), title=txt_tooltip, placement="top")), width="100%")
  })
  output$pram_simple_alpha <- renderUI({
    txt_tooltip <- "alpha allows to add some perturbation to the calculated transition matrix. The lower alpha, the less perturbed the matrix will get."
    sliderInput("pram_simple_alpha", min=0.01, max=1.00, step=0.01, value=0.5,
      label=p("Choose value for 'alpha'", tipify(icon("info-circle"), title=txt_tooltip, placement="top")), width="100%")
  })
  output$pram_simple_btn <- renderUI({
    req(input$sel_pramvars_simple, input$pram_strataV_simple)
    if (length(input$sel_pramvars_simple)==0) {
      return(NULL)
    }
    if (input$pram_strataV_simple %in% input$sel_pramvars_simple) {
      txt <- "You have selected a variable relevant for stratification that should also be postrandomized. This is not possible. Please remove the variable from one of the inputs."
      return(modalDialog(list(p(txt)), title="Error", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
    }
    myActionButton("btn_pram_nonexpert", label="Postrandomize", btn="primary")
  })
  output$pram_simple_warning <- renderUI({
    if (is.null(lastWarning())) {
      return(NULL)
    }
    fluidRow(
      column(12, h5("Application of the postrandomization attempt resulted in the following warning!", align="center")),
      column(12, verbatimTextOutput("ui_lastwarning"))
      , class = "wb-warning-toast")
  })
  output$pram_simple_error <- renderUI({
    if (is.null(lastError())) {
      return(NULL)
    }
    fluidRow(
      column(12, h5("Application of the postrandomization attempt resulted in the following warning!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))
      , class = "wb-error-toast")
  })

  pramvars <- pramVars()
  if (length(pramvars)==0) {
    return(fluidRow(
      column(12, h4("Postrandomization of categorical variables", align="center")),
      column(12, h5("No variables have been specified for postrandomization during the initialization of the current problem or
        all possible variables already have been post-randomized!", align="center"))
    ))
  }

  out <- list(uiOutput("pram_simple_error"), uiOutput("pram_simple_warning"))
  out <- list(out, fluidRow(
    column(6, uiOutput("pram_simple_var"), align="center"),
    column(6, uiOutput("pram_simple_strata"), align="center")
  ))
  out <- list(out, fluidRow(
    column(6, uiOutput("pram_simple_pd"), align="center"),
    column(6, uiOutput("pram_simple_alpha"), align="center")
  ))
  out <- list(out, fluidRow(
    column(12, uiOutput("pram_simple_btn"), align="center")
  ))
  out
})

# UI-output for kAnon
# current values of the importance-vector must be outside because of code_kAnon()
kAnon_impvec <- reactive({
  inp <- reactiveValuesToList(input)
  cn <- names(inp)[grep("sel_importance_", names(inp))]
  if (length(cn)==0) {
    return(c(""))
  }
  # we need to sort the inputs properly
  # in case >= 10 key vars have been specified
  ii <- as.numeric(sapply(strsplit(cn, "_"), utils::tail, 1))
  cn <- cn[order(ii)]
  vals <- unlist(lapply(cn, function(x) {
    inp[[x]]
  }))
  vals <- gsub("NA","", vals)
  vals
})

# values for comb-suppression
kAnon_comb_params <- reactive({
  inp <- reactiveValuesToList(input)
  cn <- names(inp)
  cn1 <- cn[grep("rb_kanon_usecombs_", cn)]

  if (length(cn1)==0) {
    return(NULL)
  }

  # we need to sort the inputs properly
  # in case >= 10 key vars have been specified
  ii <- as.numeric(sapply(strsplit(cn1, "_"), utils::tail, 1))

  cn1 <- cn1[order(ii)]

  vals1 <- unlist(lapply(cn1, function(x) {
    inp[[x]]
  }))

  cn2 <- cn[grep("sl_kanon_combs_", cn)]
  cn2 <- cn2[order(ii)]

  vals2 <- unlist(lapply(cn2, function(x) {
    inp[[x]]
  }))

  # restrict to 'active' combs
  ii <- which(vals1=="Yes")
  list(use=ii, k=vals2[ii])
})

# does the user specify a custom importance-vector?
kAnon_useImportance <- reactive({
  req(input$rb_show_importance)
  res <- FALSE
  if (input$rb_show_importance=="Yes") {
    res <- TRUE
  }
  res
})

# UI-output for kAnon()
output$ui_kAnon_header <- renderUI({
out <- fluidRow(
  column(12, h3("Establish k-anonymity"), offset = 0, class = "wb-header"),
  column(12, p("k-anonymity will be established by suppressing or rather setting some values in the selected categorical key variables to",code("NA"),"."),
             p("By default, the key variables will be considered for suppression in the order of their number of distinct categories. A variable with
                many categories is less likely to have values suppressed than a variable with few categories. It is also possible to set the order by
                specifying an importance vector."),
             p("You may also decide to apply the procedure for all possible subsets of key variables. This is useful, if you have many key variables
                and can reduce computation time. You can set a different value for the parameter",code("k"),"for each size of subsets."),
            offset = 0, class = "wb-header-hint"))
  out
})
output$ui_kAnon <- renderUI({
  # calulate current and possible values for importance-vector
  possVals_importance <- reactive({
    n <- isolate({
      length(get_keyVars())
    })

    res <- list(); length(res) <- n
    cn <- paste0('sel_importance_', 1:n)
    for ( i in 1:length(cn)) {
      res[[i]] <- list()
      val <- input[[cn[i]]]
      if (is.null(val)) {
        res[[i]]$val <- ""
      } else {
        res[[i]]$val <- val
      }
    }
    # already used:
    used <- as.numeric(na.omit(setdiff(unique(unlist(lapply(res, function(x) x$val))), c("","NA"))))
    poss <- setdiff(1:n, used)
    for ( i in 1:length(res)) {
      if ( res[[i]]$val=="") {
        res[[i]]$poss <- c("",poss)
      } else {
        res[[i]]$poss <- c("NA",poss, res[[i]]$val)
      }
    }
    res
  })

  # dynamically generate ui for importance vectors
  output$ui_kanon_importanceInputs <- renderUI({
    req(input$rb_show_importance)
    if (input$rb_show_importance=="No") {
      return(NULL)
    }

    poss <- possVals_importance()
    kV <- colnames(get_origData())[get_keyVars()]
    n <- length(kV)
    if (n == 0) {
      sl <- NULL
    } else {
      sl <- lapply(1:n, function(i) {
        selectInput(
          inputId=paste0('sel_importance_', i),
          label=paste0('Select the importance for key variable ', dQuote(kV[i])),
          choices=c("",poss[[i]]$poss), selected=poss[[i]]$val, width="100%")
      })
    }

    # how many rows?
    n_rows <- ceiling(n/3)
    out <- list()
    counter <- 1
    for (i in 1:n_rows) {
      inp1 <- sl[[counter]]
      counter <- counter+1
      if (counter > n) {
        inp2 <- NULL
      } else {
        inp2 <- sl[[counter]]
      }
      counter <- counter + 1
      if (counter > n) {
        inp3 <- NULL
      } else {
        inp3 <- sl[[counter]]
      }
      counter <- counter + 1

      out <- list(out, fluidRow(
        column(4, inp1),
        column(4, inp2),
        column(4, inp3)
      ))
    }
    out
  })

  output$ui_kanon_combs <- renderUI({
    req(input$rb_kanon_useCombs)
    out <- NULL
    if (input$rb_kanon_useCombs=="Yes") {
      nrKeyVars <- length(get_keyVars())
      out <- NULL
      # these inputs are defined when the sdcProblem is created!
      for (i in 1:nrKeyVars) {
        out <- list(out, fluidRow(
          column(6, obj$rbs[[i]], align="center"),
          column(6, obj$sls[[i]], align="center")))
      }
    } else {
      txt_tooltip <- "The choice of k is guided by the need for anonymization. A higher level of this parameter will lead to more suppressions."
      sl <- sliderInput("sl_kanon_k",
        label=p("Set the k-anonymity parameter", tipify(icon("info-circle"), title=txt_tooltip, placement="top")),
        min=2, max=50, value=3, step=1, width="100%")
      out <- fluidRow(column(12, sl, align="center"))
    }
    out
  })

  output$kanon_btn <- renderUI({
    btn <- NULL
    impvec <- kAnon_impvec()
    pp <- kAnon_comb_params()

    if (input$rb_kanon_useCombs=="Yes" && (!is.null(pp) && length(pp$use)==0)) {
      return(NULL)
    }
    if (kAnon_useImportance() && any(impvec=="")) {
      return(NULL)
    }
    btn <- myActionButton("btn_kanon", label="Establish k-anonymity", "primary")
    return(fluidRow(column(12, btn, align="center")))
  })

  output$kanon_strata <- renderUI({
    txt_tooltip <- "By default k-anonymity is established on the complete dataset. To apply the algorithm within strata, select a variable for stratification. The algorithm is then applied within the strata defined by the factor levels of that variable."
    selectInput("kanon_strataV",
      label=p("Do you want to apply the method for each group defined by the selected variable?", tipify(icon("info-circle"), title=txt_tooltip, placement="top")),
      choices=c("no stratification", setdiff(poss_strataVarP(), c(get_all_numericvars_name(), get_keyVars_names()))), multiple=FALSE, width="75%")
  })

  output$kanon_use_importance <- renderUI({
    txt_tooltip <- "Values in variables with high importance (low values) are less likely to be suppressed than values in variables with low importance (high values)"
    radioButtons(inputId="rb_show_importance",
      label=p("Do you want to modify importance of key variables for suppression?", tipify(icon("info-circle"), title=txt_tooltip, placement="top")),
      selected=input$rb_show_importance, width="100%", inline=TRUE, choices=c("No", "Yes"))
  })

  output$kanon_use_combs <- renderUI({
    txt_tooltip <- "To reduce computation time, it is possible to establish k-anonymity on all subsets of a certain size of the total set of categorical"
    txt_tooltip <- paste(txt_tooltip, "key variables. The level of k-anonymity can be set for each subset size. In case several sizes are chosen, the algorithm establishes k-anonymity first on the smaller subsets.")
    radioButtons("rb_kanon_useCombs", choices=c("No","Yes"), width="100%", inline=TRUE,
      selected=input$rb_kanon_useCombs,
      label=p("Apply k-anonymity to subsets of key variables?", tipify(icon("info-circle"), title=txt_tooltip, placement="top")))
  })

  out <- fluidRow(column(12, uiOutput("kanon_strata"), align="center"))
  out <- list(out, fluidRow(
    column(12, uiOutput("kanon_use_importance"), align="center"),
    column(12, helpText("Tip - The total number of suppressions is likely to increase by specifying an importance vector. Specifying an importance vector can affect the computation time."), align="center")
  ))
  out <- list(out, uiOutput("ui_kanon_importanceInputs")) # might be NULL

  # show combs-ui?

  out <- list(out, fluidRow(column(12, uiOutput("kanon_use_combs"), align="center")))
  out <- list(out, uiOutput("ui_kanon_combs"), uiOutput("kanon_btn"))
  out
})

# GUI-output for suppression of values in key variables with risk > than threshold
output$ui_supp_threshold_header <- renderUI({
  out <- fluidRow(
    column(12, h3("Suppress values with high risk"), offset = 0, class = "wb-header"),
    column(12, p("This method allows to suppress (set to NA) values in the selected key variables for records that have an
                 individual risk higher than the specified threshold."), offset = 0, class = "wb-header-hint"))
  out
})
output$ui_supp_threshold <- renderUI({
  output$ui_supp_th <- renderUI({
    txt_tooltip <- "Any value in the selected key variable of a record with an individual risk higher than this threshold is suppressed."
    up <- round(max(get_risk()$risk),3)+0.005
    sl <- sliderInput("sl_supp_threshold", label=p("Set threshold for individual risk", tipify(icon("info-circle"),title=txt_tooltip, placement="top")),
      min=0, max=up, value=0, step=0.001, width="100%")
    sl
  })
  output$ui_supp_riskplot <- renderPlot({
    req(input$sl_supp_threshold)
    risks <- get_risk()
    curObj <- sdcObj()
    nn <- paste(colnames(curObj@origData)[curObj@keyVars], collapse=" x ")
    hist(risks$risk, xlab="Individual Risk", ylab="Frequency", main=nn, col="#DADFE1")
    abline(v=input$sl_supp_threshold, lwd=2, col="#000000")
  })
  nr_riskyobs <- reactive({
    req(input$sl_supp_threshold)
    nr_risk <- sum(get_risk()$risk>=input$sl_supp_threshold)
    nr_risk
  })
  output$ui_supp_th_var <- renderUI({
    selectInput("sel_supp_th_var", label=p("Select key variable for suppression"), choices=get_keyVars_names(), selected=input$sel_supp_th_var, width="100%")
  })
  output$ui_supp_th_btn <- renderUI({
    req(input$sel_supp_th_var)
    btn <- myActionButton("btn_supp_th", label=paste("Suppress",nr_riskyobs(),"values with high risk in", dQuote(input$sel_supp_th_var)), "primary")
    btn
  })
  out <- fluidRow(
    column(6, uiOutput("ui_supp_th_var"), align="center"),
    column(6, uiOutput("ui_supp_th"), align="center"))
  out <- list(out, fluidRow(column(12, plotOutput("ui_supp_riskplot"))))
  out <- list(out, fluidRow(column(12, uiOutput("ui_supp_th_btn"), align="center")))
  out
})
