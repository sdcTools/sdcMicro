# UI-output for global recode
output$ui_recode <- renderUI({
  out <- fluidRow(
    column(12, h4("Recode categorical key variables", align="center")),
    column(12, p("To reduce risks it is often useful to combine multiple chararacteristics of categorical key variables into
      a new, combined category. You need to select a categorical key variable and then choose two or more levels which you want to combine. Once this
      has been done, a new label for the new category can be assigned. If you are ready, you just need to press the button.", align="center")),
    column(12, p("Note: If you only select one level, you still can press the button and update the key variable. In this case you can rename
      the the selected value.", align="center")))

  # current factor-levels
  curRecFacVals <- reactive({
    if (is.null(input$sel_recfac) ) {
      return(NULL)
    }
    if (is.null(sdcObj())) {
      return(NULL)
    }
    ff <- get_manipKeyVars()[[input$sel_recfac]]
    ll <- as.list(levels(ff))
    names(ll) <- paste(ll, "(",table(ff),"obs)")
    ll
  })

  # plot of current factor
  output$plot_facRec <- renderPlot({
    if ( is.null(input$sel_recfac) ) {
      return(NULL)
    }
    df <- table(get_manipKeyVars()[[input$sel_recfac]], useNA="always")
    dn <- dimnames(df)[[1]]
    dn[length(dn)] <- "NA"
    dimnames(df)[[1]] <- dn
    barplot(df)
  })

  # current categorical key variables
  kv <- get_keyVars_names()
  # we always have at least one categorical key-variable!
  selfac1 <- selectInput("sel_recfac",label=NULL,
    choices=kv, selected=input$sel_recfac, width="100%")
  cbgr <- selectInput("cbg_recfac",label=NULL, multiple=TRUE, selectize = FALSE,
    choices=curRecFacVals(), selected=input$cbg_recfac, width="100%")
  if (is.null(input$cbg_recfac)) {
    btnUp <- NULL
    txtval <- NULL
  } else {
    btnUp <- myActionButton("btn_update_recfac",label="Update key variable", "primary")
    txtval <- textInput("inp_newlevname_rec",label=NULL,
      value=paste0(input$cbg_recfac, collapse="_"), width="100%")
  }

  out <- list(out, fluidRow(
    column(4, h5("Choose factor variable", align="center")),
    column(4, h5("Select levels to recode/combine", align="center")),
    column(4, h5("New label for recoded values", align="center"))))
  out <- list(out, fluidRow(
    column(4, selfac1),
    column(4, cbgr),
    column(4, txtval)))
  out <- list(out, fluidRow(
    column(12, p(btnUp, align="center")),
    column(12, plotOutput("plot_facRec"))))
  out
})

# UI-output for postrandomization
output$ui_pram <- renderUI({
  # update obj$transmat if table has been changed
  observe({
    if(!is.null(input$transmat)) {
      obj$transmat <- hot_to_r(input$transmat)
    }
  })

  # Fire event when selected pram-variable changes
  # initialize transition-matrix with 0.9 in diagonals
  # because with 1 some error with rhandsontable is available.
  observeEvent(input$sel_pramvars, {
    if (!is.null(input$sel_pramvars) && length(input$sel_pramvars)==1) {
      v <- get_origData()[[input$sel_pramvars]]
      ll <- levels(v)
      m <- diag(length(ll))
      diag(m) <- 0.90
      rownames(m) <- colnames(m) <- ll
      obj$transmat <- m
    }
  })

  # Transitionmatrix for PRAM
  output$transmat <- renderRHandsontable({
    if (is.null(input$sel_pramvars) || length(input$sel_pramvars)!=1) {
      return(NULL)
    }
    # hot_col(1:ncol(obj$transmat), type="numeric", format="0.00")
    m <- rhandsontable(obj$transmat) #%>% hot_context_menu(allowRowEdit=FALSE, allowColEdit=FALSE)
    m
  })

  output$ui_pram_params <- renderUI({
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(NULL)
    }
    pramvars <- setdiff(facVars(), curObj@pram$summary$variable)

    if (length(pramvars)==0) {
      return(fluidRow(
        column(12, h4("Postrandomization of categorical variables", align="center")),
        column(12, h5("No factor variables available in the data, or all possible variables already have been post-randomized!", align="center"))
      ))
    }

    rb_expert <- radioButtons("rb_expert_pram", label=h5("Use expert settings"),
      choices=c(FALSE, TRUE), selected=input$rb_expert_pram, inline=TRUE)

    if (is.null(input$rb_expert_pram) || input$rb_expert_pram==FALSE) {
      sel_pramvar <- selectInput("sel_pramvars", choices=pramvars, label=h5("Select variable(s) for PRAM"),
        selected=input$sel_pramvars, width="100%", multiple=TRUE)
    } else {
      sel_pramvar <- selectInput("sel_pramvars", choices=pramvars, label=h5("Select variable for PRAM"),
        selected=input$sel_pramvars, width="100%", multiple=FALSE)
    }

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
      column(12, h4("Postrandomization of categorical variables", align="center")),
      column(12, p("The algorithm randomly changes the values of selected variables in some records according
      to an invariant probability transition matrix (in non-expert mode) or a custom-defined transition matrix (in expert mode).", align="center")),
      column(12, p("In non-expert mode, two parameters (",code("pd"),"and",code("alpha"),") must be specified.",
        code("pd"),"refers to the minimum diagonal values in the (internally) generated transition matrix. The higher this
      value is chosen, the more likely it is that a value stays the same and is not going to be changed.",code("alpha"),"allows to add some
      perturbation to the calculated transition matrix. The lower this number is, the less perturbed the matrix will get. In
      expert mode, the user can freely specify a transition matrix which will be used for the post-randomization of a single variable. However, the
      requirement is that all row sums of the specified matrix sum up to 1!", align="center")),
      column(12, p("Please also note that if you have specified a stratification variable when creating the",code("sdcMicroObj"),"postrandomization
      is performed independently on all data-subsets specified by the stratification variable!", align="center"))))

    out <- list(out, fluidRow(
      column(6, rb_expert, align="center"),
      column(6, sel_pramvar, align="center")))

    if (is.null(input$rb_expert_pram) || input$rb_expert_pram==FALSE) {
      sl1 <- sliderInput("sl_pd", min=0.01, max=1.00, step=0.01, value=0.8, label=h5("Choose value for 'pd'"), width="100%")
      sl2 <- sliderInput("sl_alpha", min=0.01, max=1.00, step=0.01, value=0.5,label=h5("Choose value for 'alpha'"), width="100%")
      btn_submit <- myActionButton("btn_pram_nonexpert", label="Postrandomize", btn="primary")
      out <- list(out, fluidRow(
        column(6, sl1, align="center"),
        column(6, sl2, align="center")))
      out <- list(out, fluidRow(
        column(12, btn_submit, align="center")))
    } else {
      if (is.null(obj$transmat)) {
        btn_submit <- NULL
      } else {
        btn_submit <- myActionButton("btn_pram_expert", label="Postrandomize", btn="primary")
        if (!all(rowSums(obj$transmat)==1)) {
          btn_submit <- myActionButton("btn_pram_expert_notworking", label="Error: Not all row-sums of the transition matrix equal 1", btn="danger")
        }
      }

      out <- list(out, fluidRow(
        column(12, rHandsontableOutput("transmat", width="100%")),
        column(12, btn_submit, align="center")))
    }
    out
    # if (input$rb_expert_pram==TRUE) {
    #   if (length(pramvars) > 0) {
    #     btn_submit <- myActionButton("btn_pram_expert", label="Postrandomize", btn="primary")
    #     if (!all(rowSums(obj$transmat)==1)) {
    #       btn_submit <- myActionButton("btn_pram_expert_notworking", label="Error: Not all row-sums of the transition matrix equal 1", btn="danger")
    #     }
    #     out <- list(out, fluidRow(
    #       column(12, rHandsontableOutput("transmat", width="100%")),
    #       column(12, btn_submit, align="center")))
    #     return(out)
    #   }
    # } else {
    #   sl1 <- sliderInput("sl_pd", min=0.01, max=1.00, step=0.01, value=0.8, label=h5("Choose value for 'pd'"), width="100%")
    #   sl2 <- sliderInput("sl_alpha", min=0.01, max=1.00, step=0.01, value=0.5,label=h5("Choose value for 'alpha'"), width="100%")
    #   btn_submit <- myActionButton("btn_pram_nonexpert", label="Postrandomize", btn="primary")
    #   out <- list(out, fluidRow(
    #     column(6, sl1, align="center"),
    #     column(6, sl2, align="center")))
    #   out <- list(out, fluidRow(
    #     column(12, btn_submit, align="center")))
    #   return(out)
    # }
  })

  #out <- fluidRow(
  #  column(12, h4("Postrandomization of categorical variables", align="center")),
  #  column(12, p("The algorithm randomly changes the values of selected variables in some records according
  #    to an invariant probability transition matrix (in non-expert mode) or a custom-defined transition matrix (in expert mode).", align="center")),
  #  column(12, p("In non-expert mode, two parameters (",code("pd"),"and",code("alpha"),") must be specified.",
  #    code("pd"),"refers to the minimum diagonal values in the (internally) generated transition matrix. The higher this
  #    value is chosen, the more likely it is that a value stays the same and is not going to be changed.",code("alpha"),"allows to add some
  #    perturbation to the calculated transition matrix. The lower this number is, the less perturbed the matrix will get. In
  #    expert mode, the user can freely specify a transition matrix which will be used for the post-randomization of a single variable. However, the
  #    requirement is that all row sums of the specified matrix sum up to 1!", align="center")),
  #  column(12, p("Please also note that if you have specified a stratification variable when creating the",code("sdcMicroObj"),"postrandomization
  #    is performed independently on all data-subsets specified by the stratification variable!", align="center")))

  #out <- list(out, fluidRow(
  #  column(6, uiOutput("pram_useExpert"), align="center"),
  #  column(6, uiOutput("pram_var"), align="center")))
  out <- list(uiOutput("ui_pram_params"))
  out
})

# UI-output for kAnon
# current values of the importance-vector
# must be outside because of code_kAnon()
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
        label=paste0('Select the Importance for keyVariable ', kV[i]),
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
    if (!kAnon_useImportance() | (all(impvec!="") & (is.null(pp) | length(pp$use)>0))) {
      btn <- myActionButton("btn_kanon", label="Establish k-Anonymity", "primary")
    }
    return(fluidRow(column(12, btn, align="center")))
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
  # so that the slider-value is not changed while updating the importance vector
  output$ui_supp_th <- renderUI({
    up <- round(max(get_risk()$risk),3)+0.005
    sl <- sliderInput("sl_supp_threshold", label=h5("Individual Risk Threshold"),
      min=0, max=up, value=0, step=0.001, width="100%")
    sl
  })
  # risk-plot
  output$ui_supp_riskplot <- renderPlot({
    if ( is.null(input$sl_supp_threshold)) {
      return(NULL)
    }
    risks <- get_risk()
    curObj <- sdcObj()
    nn <- paste(colnames(curObj@origData)[curObj@keyVars], collapse=" x ")
    hist(risks$risk, xlab="Individual Risks", ylab="Frequency", main=nn, col="#DADFE1")
    abline(v=input$sl_supp_threshold, lwd=2, col="#F9690E")
  })
  # number of risky observations that would be suppressed
  nr_riskyobs <- reactive({
    req(input$sl_supp_threshold)
    nr_risk <- sum(get_risk()$risk>=input$sl_supp_threshold)
    nr_risk
  })
  # key-variable
  output$ui_supp_th_var <- renderUI({
    req(input$sl_supp_threshold)
    selectInput("sel_supp_th_var", label=h5("Select Key-Variable for Suppression"), choices=get_keyVars_names(), selected=input$sel_supp_th_var, width="100%")
  })

  output$ui_supp_th_btn <- renderUI({
    req(input$sel_supp_th_var)
    btn <- myActionButton("btn_supp_th", label=paste("Supress",nr_riskyobs(),"values with high risk in", dQuote(input$sel_supp_th_var)), "primary")
    btn
  })

  out <- fluidRow(
    column(12, h4("Supress above given threshold", align="center")),
    column(12, p("This is a relatively easy method which allows to suppress or rather set to",code("NA"),"values in selected key-variables
      in observations that have a individual risk higher than the selected risk-threshold. Please note that this method does not take into account a possibly
      specified stratification variable.", align="center")))

  out <- list(out, fluidRow(
    column(6, uiOutput("ui_supp_th")),
    column(6, uiOutput("ui_supp_th_var"))))
  out <- list(out, fluidRow(
    column(12, plotOutput("ui_supp_riskplot"))))
  out <- list(out, fluidRow(
    column(12, uiOutput("ui_supp_th_btn"), align="center")))
  out
})
