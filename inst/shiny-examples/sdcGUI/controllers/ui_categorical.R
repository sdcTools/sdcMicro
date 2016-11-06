# UI-output for global recode
output$ui_recode <- renderUI({
  out <- fluidRow(
    column(12, h4("Recode categorical key variables", align="center")),
    column(12, p("To reduce risk, it is often useful to combine multiple chararacteristics of categorical key variables into
      a new, combined category. You need to select a categorical key variable and then choose two or more levels which you want to combine. Once this
      has been done, a new label for the new category can be assigned. If you are ready, you just need to press the button.", align="center")),
    column(12, p("Note: If you only select one level, you still can press the button and update the key variable. In this case you can rename
      the the selected value.", align="center")))

  # current factor-levels
  curRecFacVals <- reactive({
    req(input$sel_recfac)
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(NULL)
    }
    ff <- get_manipKeyVars()[[input$sel_recfac]]
    ll <- as.list(levels(ff))
    ii <- which(is.na(ll))
    if (length(ii)==1) {
      ll[[ii]] <- "NA"
    }
    names(ll) <- paste(ll, "(",table(ff),"obs)")
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
    # we always have at least one categorical key-variable!
    selfac1 <- selectInput("sel_recfac",label=h5("Choose factor variable"), choices=kv, selected=input$sel_recfac, width="100%")
    selfac1
  })
  output$recfac_cbgr <- renderUI({
    cbgr <- selectInput("cbg_recfac",label=h5("Select levels to recode/combine"), multiple=TRUE, selectize=TRUE,
      choices=curRecFacVals(), width="100%")
  })
  output$recfac_btn <- renderUI({
    req(input$cbg_recfac)
    myActionButton("btn_update_recfac",label="Update key variable", "primary")
  })
  output$recfac_txtval <- renderUI({
    req(input$cbg_recfac)
    txtval <- textInput("inp_newlevname_rec",label=h5("New label for recoded values"),
      value=paste0(input$cbg_recfac, collapse="_"), width="100%")
  })
  out <- list(out, fluidRow(
    column(4, uiOutput("recfac_selfac1"), align="center"),
    column(4, uiOutput("recfac_cbgr"), align="center"),
    column(4, uiOutput("recfac_txtval"), align="center")))
  out <- list(out, fluidRow(
    column(12, uiOutput("recfac_btn"), align="center"),
    column(12, plotOutput("plot_facRec"))))
  out
})

# UI-output for postrandomization (expert-useage)
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
    # hot_col(1:ncol(obj$transmat), type="numeric", format="0.00")
    m <- rhandsontable(obj$transmat) #%>% hot_context_menu(allowRowEdit=FALSE, allowColEdit=FALSE)
    m
  })
  output$pram_expert_strata <- renderUI({
    selectInput("pram_expert_strataV", label=h5("Postrandomize within different groups (stratification)?"),
      choices=c("no stratification", poss_strataVarP()), multiple=FALSE, width="100%")
  })
  output$pram_expert_var <- renderUI({
    selectInput("sel_pramvars_expert", choices=pramVars(), label=h5("Select variable for PRAM"),
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
      txt <- "You have selected a variable relevant for stratification that should also be pramed. This is not possible. Please remove the variable from one of the inputs"
      return(modalDialog(list(p(txt)), title="Error", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
    }
    return(myActionButton("btn_pram_expert", label="Postrandomize", btn="primary"))
  })

  if (length(pramVars())==0) {
    return(fluidRow(
      column(12, h4("Postrandomization of categorical variables", align="center")),
      column(12, h5("No factor variables available in the data, or all possible variables already have been post-randomized!", align="center"))
    ))
  }

  out <- NULL
  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Application of the last Postrandomization attempt resulted in the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))))
  }
  if (!is.null(lastWarning())) {
    out <- list(out, fluidRow(
      column(12, inp=h4("Application of the last Postrandomization attempt resulted in the following warning!", align="center")),
      column(12, inp=verbatimTextOutput("ui_lastwarning"))))
  }
  out <- list(out, fluidRow(
    column(12, h4("Postrandomization of categorical variables (expert usage)", align="center")),
    column(12, p("The algorithm allows to randomly change values in the selected variable according to a custom-defined transition matrix.", align="center")),
    column(12, p("Below, you can now freely specify a transition matrix which will be used for the post randomization of a single variable. You need to make sure
      that all row sums of the specified matrix sum up to 1!", align="center")),
    column(12, p("Please also note that you may specify a stratification variable. In this case, the postrandomization
      is performed independently on all data-subsets specified by the selected stratification variable!", align="center"))))

  out <- list(out, fluidRow(
    column(6, uiOutput("pram_expert_var"), align="center"),
    column(6, uiOutput("pram_expert_strata"), align="center")))

  out <- list(out, fluidRow(
    column(12, rHandsontableOutput("pram_expert_transmat", width="100%")),
    column(12, tags$br(), uiOutput("pram_expert_btn"), align="center")))
  out
})

# UI-output for postrandomization (simple-useage)
output$ui_pram_simple <- renderUI({
  output$pram_simple_var <- renderUI({
    selectInput("sel_pramvars_simple", choices=pramVars(), label=h5("Select variable(s) for PRAM"), width="100%", multiple=TRUE)
  })
  output$pram_simple_strata <- renderUI({
    selectInput("pram_strataV_simple", label=h5("Postrandomize within different groups (stratification)?"),
      choices=c("no stratification", poss_strataVarP()), multiple=FALSE, width="100%")
  })
  output$pram_simple_pd <- renderUI({
    sliderInput("pram_simple_pd", min=0.01, max=1.00, step=0.01, value=0.8, label=h5("Choose value for 'pd'"), width="100%")
  })
  output$pram_simple_alpha <- renderUI({
    sliderInput("pram_simple_alpha", min=0.01, max=1.00, step=0.01, value=0.5,label=h5("Choose value for 'alpha'"), width="100%")
  })
  output$pram_simple_btn <- renderUI({
    req(input$sel_pramvars_simple, input$pram_strataV_simple)
    if (length(input$sel_pramvars_simple)==0) {
      return(NULL)
    }
    if (input$pram_strataV_simple %in% input$sel_pramvars_simple) {
      txt <- "You have selected a variable relevant for stratification that should also be pramed. This is not possible. Please remove the variable from one of the inputs"
      return(modalDialog(list(p(txt)), title="Error", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
    }
    myActionButton("btn_pram_nonexpert", label="Postrandomize", btn="primary")
  })

  pramvars <- pramVars()
  if (length(pramvars)==0) {
    return(fluidRow(
      column(12, h4("Postrandomization of categorical variables", align="center")),
      column(12, h5("No factor variables available in the data, or all possible variables already have been post-randomized!", align="center"))
    ))
  }

  out <- NULL
  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("Application of the Postrandomization attempt resulted in the following error!", align="center")),
      column(12, verbatimTextOutput("ui_lasterror"))))
  }
  if (!is.null(lastWarning())) {
    out <- list(out, fluidRow(
      column(12, h4("Application of the Postrandomization attempt resulted in the following warning!", align="center")),
      column(12, verbatimTextOutput("ui_lastwarning"))))
  }

  out <- list(out, fluidRow(
    column(12, h4("Postrandomization of categorical variables (simple)", align="center")),
    column(12, p("The algorithm randomly changes the values of selected variables in some records according
      to an invariant probability transition matrix (in non-expert mode).", align="center")),
    column(12, p("To generate the transition matrix, two parameters (",code("pd"),"and",code("alpha"),") must be specified.",
      code("pd"),"refers to the minimum diagonal values in the (internally) generated transition matrix. The higher this
      value is chosen, the more likely it is that a value stays the same and is not going to be changed.",code("alpha"),"allows to add some
      perturbation to the calculated transition matrix. The lower this number is, the less perturbed the matrix will get.", align="center")),
    column(12, p("Please also note that you may specify a stratification variable. In this case, the postrandomization
      is performed independently on all data-subsets specified by the selected stratification variable!", align="center"))))

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
  cn <- names(input)[grep("sel_importance_", names(input))]
  if (length(cn)==0) {
    return(c(""))
  }
  vals <- unlist(lapply(cn, function(x) {
    input[[x]]
  }))
  vals <- gsub("NA","", vals)
  vals
})

# values for comb-suppression
kAnon_comb_params <- reactive({
  cn <- names(input)

  cn1 <- cn[grep("rb_kanon_usecombs_", cn)]
  if (length(cn1)==0) {
    return(NULL)
  }
  vals1 <- unlist(lapply(cn1, function(x) {
    input[[x]]
  }))

  cn2 <- cn[grep("sl_kanon_combs_", cn)]
  vals2 <- unlist(lapply(cn2, function(x) {
    input[[x]]
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
    sl <- lapply(1:n, function(i) {
      selectInput(
        inputId=paste0('sel_importance_', i),
        label=paste0('Select the importance for key variable ', dQuote(kV[i])),
        choices=c("",poss[[i]]$poss), selected=poss[[i]]$val, width="100%")
    })
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
      sl <- sliderInput("sl_kanon_k", label=h5("Please specify the k-Anonymity parameter"),
        min=2, max=50, value=3, step=1, width="100%")
      out <- fluidRow(column(12, sl, align="center"))
    }
    out
  })

  output$kanon_btn <- renderUI({
    btn <- NULL
    impvec <- kAnon_impvec()
    pp <- kAnon_comb_params()
    if (!is.null(pp) && length(pp$use)==0) {
      return(NULL)
    }
    if (kAnon_useImportance() && any(impvec=="")) {
      return(NULL)
    }
    btn <- myActionButton("btn_kanon", label="Establish k-Anonymity", "primary")
    return(fluidRow(column(12, btn, align="center")))
  })

  output$kanon_strata <- renderUI({
    selectInput("kanon_strataV", label=h5("Do you want to apply the method for each group defined by the selected variable?"),
      choices=c("no stratification", setdiff(poss_strataVarP(), c(get_all_numericvars_name(), get_keyVars_names()))), multiple=FALSE, width="75%")
  })

  out <- fluidRow(
    column(12, h4("Establish k-anonymity", align="center")),
    column(12, p("k-Anonymity will be established by suppressing or rather setting to",code(NA),"some values in the categorical key variables.
    By default, the key-variables will be preferred to feature required suppressions will be computed by default. If you decide to specify the",tags$i("importance"),", make
    sure that assign low numbers to those variables that you do not want many suppressions in. On the other hand, the higher the importance value for a variable, the larger
    the probability that values will be suppressed within this variable", align="center")),
    column(12, p("You may also decide to apply the procedure for all possible subsets of key variables. This is useful, if you have many
      key variables. In this case you can choose different values for parameter",code("k"),".", align="center")),
    column(12, p("Please also note that if you have specified a stratification variable when creating the",code("sdcMicroObj"),"k-anonymity is established
      for all the data-subsets specified by the stratification variable!", align="center"))
  )

  out <- list(out, fluidRow(column(12, uiOutput("kanon_strata"), align="center")))

  rb1 <- radioButtons(inputId="rb_show_importance", label=h5(paste("Do you want to modify importance of key-variables for suppression?")),
    selected=input$rb_show_importance, width="100%", inline=TRUE, choices=c("No", "Yes"))
  out <- list(out, fluidRow(column(12, rb1, align="center")))

  out <- list(out, uiOutput("ui_kanon_importanceInputs")) # might be NULL

  # show combs-ui?
  rb2 <- radioButtons("rb_kanon_useCombs", choices=c("No","Yes"), width="100%", inline=TRUE,
    selected=input$rb_kanon_useCombs, label=h5("Apply k-Anonymity to subsets of key-variables?"))
  out <- list(out, fluidRow(column(12, rb2, align="center")))
  out <- list(out, uiOutput("ui_kanon_combs"), uiOutput("kanon_btn"))
  out
})

# GUI-output for suppression of values in key-variables with risk > than threshold
output$ui_supp_threshold <- renderUI({
  output$ui_supp_th <- renderUI({
    up <- round(max(get_risk()$risk),3)+0.005
    sl <- sliderInput("sl_supp_threshold", label=h5("Individual Risk Threshold"),
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
    selectInput("sel_supp_th_var", label=h5("Select Key-Variable for Suppression"), choices=get_keyVars_names(), selected=input$sel_supp_th_var, width="100%")
  })
  output$ui_supp_th_btn <- renderUI({
    req(input$sel_supp_th_var)
    btn <- myActionButton("btn_supp_th", label=paste("Suppress",nr_riskyobs(),"values with high risk in", dQuote(input$sel_supp_th_var)), "primary")
    btn
  })

  out <- fluidRow(
    column(12, h4("Suppress above given threshold", align="center")),
    column(12, p("This is a relatively easy method which allows to suppress or rather set to",code("NA"),"values in selected key-variables
      in observations that have a individual risk higher than the selected risk-threshold. Please note that this method does not take into account a possibly
      specified stratification variable.", align="center")))
  out <- list(out, fluidRow(
    column(6, uiOutput("ui_supp_th_var")),
    column(6, uiOutput("ui_supp_th"))))
  out <- list(out, fluidRow(column(12, plotOutput("ui_supp_riskplot"))))
  out <- list(out, fluidRow(column(12, uiOutput("ui_supp_th_btn"), align="center")))
  out
})
