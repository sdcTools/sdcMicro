# UI-output for recoding a variable to a numeric variable
output$ui_modify_recode_to_numeric <- renderUI({
  output$ui_to_num_var <- renderUI({
    selectInput("sel_to_num_var",label=h5("Choose variable"), choices=vv, width="50%", selected=input$sel_to_num_var, multiple=TRUE)
  })
  output$ui_to_num_btn <- renderUI({
    req(input$sel_to_num_var)
    if (length(input$sel_to_num_var)>0) {
      return(myActionButton("btn_recode_to_numeric",label=("Recode to numeric"), "primary"))
    }
  })

  vv <- c(charVars(), facVars())
  if (length(vv) == 0) {
    return(fluidRow(
      column(12, h4("No character/factor variables available in the inputdata!", align="center"))
    ))
  }
  out <- fluidRow(
    column(12, h4("Recode a character/factor variable(s) into a numeric variable"), align="center"),
    column(12, p("You can now choose a variable of class 'character' or 'factor' and convert it into a variable of type 'numeric'"), align="center"))
  out <- list(out, fluidRow(
    column(12, uiOutput("ui_to_num_var"), align="center"),
    column(12, uiOutput("ui_to_num_btn"), align="center")))
  out
})

# UI-output for recoding a variable to a factor
output$ui_modify_recode_to_factor <- renderUI({
  get_current_custom_vars <- reactive({
    if (is.null(input$sel_custom_split)) {
      return(NULL)
    }
    if (input$sel_custom_split=="no") {
      return(NULL)
    } else {
      return(input$rb_num_glrec)
    }
  })
  summary_globalrec <- reactive({
    vv <- get_current_custom_vars()
    if (is.null(vv)) {
      return(NULL)
    }
    out <- as.data.frame(table(obj$inputdata[[vv]], useNA="always"))
    colnames(out) <- c("Value", "Frequency")
    out$Frequency <- as.integer(out$Frequency)
    out
  })
  output$ui_globalRecode_auto <- renderUI({
    if (is.null(input$sel_algo)){
      return(NULL)
    }
    sl1 <- numericInput("sl_number_breaks",label=h5("Specify the number of breaks"), value=3, min=2, max=10, step=1)
    sl1
  })
  output$ui_globalRecode_manual <- renderUI({
    if (is.null(input$sel_algo)){
      return(NULL)
    }
    sel_br <- customTextInput("txt_custom_breaks",label=h5("Specify the custom breaks"), value=input$txt_custom_breaks)
    help_br <- helpText(
      "Example input: 1,3,5,9 splits the variable into the 3 groups (1,3],(3,5] and (5,9].", br(),
      "If you supply 1 number (e.g. 3), the variable will be split in 3 equal sized groups.")
    list(sel_br, tags$br(), help_br)
  })
  output$ui_globalRecode_custom <- renderUI({
    if (!is.null(input$sel_algo) && input$sel_algo!="manual") {
      out <- fluidRow(
        column(6, uiOutput("ui_globalRecode_cutalgo"), align="center"),
        column(6, uiOutput("ui_globalRecode_auto"), align="center"))
    } else {
      out <- fluidRow(
        column(6, uiOutput("ui_globalRecode_cutalgo"), align="center"),
        column(6, uiOutput("ui_globalRecode_manual"), align="center"))
    }
    out
  })
  output$ui_globalRecode_var <- renderUI({
    req(input$sel_custom_split)
    mult <- FALSE
    if (!is.null(input$sel_custom_split) && input$sel_custom_split=="no") {
      selectInput("sel_num_glrec",label=h5("Choose numeric variable"), choices=vv, selected=input$sel_num_glrec, multiple=TRUE)
    } else {
      radioButtons("rb_num_glrec",label=h5("Choose numeric variable"), choices=vv, selected=input$rb_num_glrec)
    }
  })
  output$ui_globalRecode_split <- renderUI({
    radioButtons("sel_custom_split",label=h5("Custom Breaks"), choices=c("no","yes"),
      selected=input$sel_custom_split, inline=TRUE, width="100%")
  })
  output$ui_globalRecode_cutalgo <- renderUI({
    selectInput("sel_algo",label=h5("Select Algorithm"),
      choices=c("equidistant","logEqui","equalAmount","manual"), selected=input$sel_algo)
  })
  output$ui_globalRecode_btn <- renderUI({
    if (is.null(input$sel_custom_split)) {
      return(NULL)
    }
    btn_rec <- myActionButton("btn_recode_to_factor",label=("Recode to factor"), "primary")
    if (input$sel_custom_split=="no") {
      return(btn_rec)
    } else {
      if (!is.null(input$sel_algo) && input$sel_algo!="manual") {
        return(btn_rec)
      } else {
        inp <- input$txt_custom_breaks
        if (is.null(inp) || inp=="") {
          return(NULL)
        }
        res <- suppressWarnings(as.numeric(unlist(strsplit(inp, ","))))
        if (sum(is.na(res))>0) {
          return(fluidRow(
            column(12, myActionButton("btn_recode_to_factor_notworking",label=("Error: Please check your input (non-numeric?)"), "danger"), align="center")
          ))
        } else if (length(res)==1 & res > nrow(obj$inputdata)) {
          return(fluidRow(
            column(12, myActionButton("btn_recode_to_factor_notworking",label=("Error: The number of groups is too large!"), "danger"), align="center")
          ))
        } else {
          return(btn_rec)
        }
      }
    }
  })
  output$ui_globalRecode_summary <- renderUI({
    ss <- summary_globalrec()
    if (is.null(ss)) {
      return(NULL)
    }
    out <- fluidRow(column(12, renderTable(ss, row.names=FALSE), align="center"))
    out
  })

  vv <- numVars()
  if (length(vv) == 0) {
    return(fluidRow(
      column(12, h4("No numeric variables available in the inputdata!", align="center"))
    ))
  }

  out <- fluidRow(
    column(12, h4("Recode a numeric variable into a factor", align="center"),
    column(12, p("You can now choose a variable of class 'numeric' and convert it into a variable of type 'factor'. If
      you do not want to use custom breaks, the variable is transformed without any changes into a factor variable and all unique values
      of the variable will also be factor levels. By using custom-breaks, you need to either specify the number of new categories or
      define the cut-points manually."), align="center"),
    column(12, p("Please note that factor labels are automatically generated. You can change them later once the factor has been generated!"), align="center")))

  if (!is.null(input$sel_custom_split) && input$sel_custom_split=="yes") {
    out <- list(out, fluidRow(
      column(4, uiOutput("ui_globalRecode_var"), align="center"),
      column(4, uiOutput("ui_globalRecode_btn"), align="center"),
      column(4, uiOutput("ui_globalRecode_split"), align="center")))
    out <- list(out, uiOutput("ui_globalRecode_custom"), uiOutput("ui_globalRecode_summary"))
  } else {
    out <- list(out, fluidRow(
      column(6, uiOutput("ui_globalRecode_var"), align="center"),
      column(6, uiOutput("ui_globalRecode_split"), align="center")))
    out <- list(out, uiOutput("ui_globalRecode_summary"))
    out <- list(out, fluidRow(
      column(12, uiOutput("ui_globalRecode_btn"), align="center")))
  }
  out
})

# UI-output for modifying a factor-variable
output$ui_modify_change_factor <- renderUI({
  # plot of current factor
  output$plot_fac <- renderPlot({
    if ( is.null(input$sel_factor) ) {
      return(NULL)
    }
    barplot(table(obj$inputdata[[input$sel_factor]]))
  })

  # current factor-levels
  curFactorVals <- reactive({
    if (is.null(input$sel_factor) ) {
      return(NULL)
    }
    ff <- obj$inputdata[[input$sel_factor]]
    ll <- as.list(levels(ff))
    names(ll) <- paste(ll, "(",table(ff),"obs)")
    ll
  })

  vv <- facVars()
  out <- fluidRow(column(12, h4("Modify an existing factor-variable", align="center")))
  if (length(vv)==0) {
    out <- list(out, fluidRow(
      column(12, h5("Currently, there are no factor-variables available that could be recoded!", align="center"))
    ))
    return(out)
  }
  selfac1 <- selectInput("sel_factor",label=NULL,
    choices=vv, selected=input$sel_factor, width="100%")
  cbgr <- selectInput("cbg_factor",label=NULL, multiple=TRUE, selectize=FALSE,
    choices=curFactorVals(), selected=input$cbg_factor, width="100%")
  if (is.null(input$cbg_factor)) {
    btnUp <- NULL
    txtval <- NULL
  } else {
    btnUp <- myActionButton("btn_update_factor",label="Update factor", "primary")
    txtval <- textInput("inp_newlevname",label=NULL,
    value=paste0(input$cbg_factor, collapse="_"), width="100%")
  }

  out <- list(out, fluidRow(
    column(4, h5("Choose factor variable", align="center")),
    column(4, h5("Select Levels to recode/combine", align="center")),
    column(4, h5("New label for recoded values", align="center"))))

  out <- list(out, fluidRow(
    column(4, selfac1, align="center"),
    column(4, cbgr, align="center"),
    column(4, txtval, align="center")))
  out <- list(out, fluidRow(
    column(12, btnUp, align="center"),
    column(12, plotOutput("plot_fac"))))
  out
})

# UI-output to create a stratification variable
output$ui_modify_create_stratvar <- renderUI({
  sel <- selectInput("sel_allvars_strata",label=h5("Select Variables"), multiple=TRUE,
    choices=allVars(), selected=input$sel_allvars_strata, width="100%")

  if ( is.null(input$sel_allvars_strata) ) {
    txtval <- NULL
    btn <- NULL
  } else {
    txtval <- textInput("inp_vname_strata",label=h5("desired variable name"),
      value=paste0(input$sel_allvars_strata, collapse="_"), width="100%")
    btn <- myActionButton("btn_create_stratavar",label=("Create stratification variable"), "primary")
  }

  out <- fluidRow(
    column(12, h4("Create a stratification variable", align="center")),
    column(12, p("This method allows to",tags$i("chain together"),"values of two
      or more variables which will be seperated by",code("_"),". You can also specify the variable name by typing
      it into the text-field and which will be used to append the new variable to the micro data set.
      This is useful if you want to create a new variable for e.g stratification purposes when creating a new sdc pproblem.", align="center")))

  out <- list(out, fluidRow(
    column(6, p(sel, align="center")),
    column(6, p(txtval, align="center"))))

  out <- list(out, fluidRow(
    column(6, p(btn, align="center"))))
  out
})

# UI-output to set specific values to NA
output$ui_set_to_na <- renderUI({
  output$ui_nasupptype <- renderUI({
    radioButtons("set_to_na_type", label=h5("How do you want to select the Observations where you want to set values to missing?"),
      choices=c("By Id"="id", "By rule"="rule"), selected=input$set_to_na_type, inline=TRUE)
  })
  output$tab_inputdata_setna <- renderDataTable({
    a <- obj$inputdata
    cbind(id=1:nrow(a),a)
  },
    options = list(scrollX=TRUE, searching=FALSE, paging=TRUE, ordering=FALSE, bInfo=FALSE))

  output$ui_nasuppvar <- renderUI({
    if (is.null(input$set_to_na_type)) {
      return(NULL)
    }
    multiple <- FALSE
    if (input$set_to_na_type=="id") {
      multiple <- TRUE
    }
    res <- selectInput("sel_na_suppvar", choices=allVars(), multiple=multiple, label="Select Variable for Suppression")
    return(res)
  })

  output$ui_nasuppid <- renderUI({
    if (is.null(input$sel_na_suppvar)) {
      return(NULL)
    }
    if (input$set_to_na_type=="id") {
      res <- numericInput("num_na_suppid", label="In which ID do you want to suppress values?", value=1, min=1, max=nrow(obj$inputdata))
    } else {
      res <- selectInput("num_na_suppid", label="Which value in this variable would you like to set to NA", multiple=FALSE, choices=sort(unique(obj$inputdata[[input$sel_na_suppvar]])))
    }
    return(res)
  })


  output$ui_ansuppbtn <- renderUI({
    if (is.null(input$sel_na_suppvar) || length(input$sel_na_suppvar)==0) {
      return(NULL)
    }
    if (is.null(input$num_na_suppid) || length(input$num_na_suppid)==0) {
      return(NULL)
    }
    myActionButton("btn_set_to_na",label=("Set values to NA"), "primary")
  })

  out <- fluidRow(
    column(12, h4("Set cells to NA (missing)", align="center")),
    column(12, uiOutput("ui_nasupptype")))

  out <- list(out, fluidRow(
    column(6, uiOutput("ui_nasuppvar"), align="center"),
    column(6, uiOutput("ui_nasuppid"), align="center")))
  out <- list(out, fluidRow(
    column(12, uiOutput("ui_ansuppbtn"), align="center"),
    column(12, dataTableOutput("tab_inputdata_setna"), align="center")))
  out
})

# UI-output to display a variable
# users can choose a summary or a plot which depends
# on the class of the variable
output$ui_view_var <- renderUI({
  output$ui_selvar1 <- renderUI({
    selectInput("view_selvar1", choices=allVars(), label=h5("Choose a variable"), multiple=FALSE, selected=obj$inp_sel_viewvar1, width="100%")
  })
  # This is required so that usual changes of the dropdown-select are also reflected in the reactive variable obj$inp_sel_viewvar1
  observeEvent(input$view_selvar1, {
    obj$inp_sel_viewvar1 <- input$view_selvar1
  })
  output$ui_selvar2 <- renderUI({
    selectInput("view_selvar2", choices=c("none", allVars()), label=h5("Choose a second variable (optional)"), multiple=FALSE, width="100%")
  })

  observeEvent(input$view_selvar1, {
    vv <- allVars()
    ii <- which(input$view_selvar1==vv)
    if (length(ii)>0) {
      vv <- c("none",vv[-c(ii)])
      updateSelectInput(session, inputId="view_selvar2", choices=vv, selected=input$view_selvar2)
    }
  })
  observeEvent(input$view_selvar2, {
    vv <- allVars()
    ii <- which(input$view_selvar2==vv)
    if (length(ii)>0) {
      vv <- vv[-c(ii)]
      updateSelectInput(session, inputId="view_selvar1", choices=vv, selected=input$view_selvar1)
    }
  })

  output$view_summary <- renderUI({
    req(input$view_selvar1, input$view_selvar2)
    inputdata <- inputdata()
    if(is.null(inputdata)) {
      return(NULL)
    }

    v1 <- input$view_selvar1
    v2 <- input$view_selvar2
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
        vcor <- round(cor(df[[1]], df[[2]]),3)
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
  output$view_plot <- renderPlot({
    inputdata <- inputdata()
    req(input$view_selvar1, input$view_selvar2)

    vv1 <- inputdata[[input$view_selvar1]]
    if (input$view_selvar2=="none") {
      if (is.factor(vv1) | is.character(vv1)) {
        tt <- table(vv1, useNA="always")
        names(tt)[length(tt)] <- "NA"
        barplot(tt, col="#DADFE1")
      } else {
        hist(vv1, main=NULL, xlab=input$view_selvar1, col="#DADFE1")
      }
    } else {
      vv2 <- inputdata[[input$view_selvar2]]
      cl1 <- class(vv1) %in% c("factor", "character")
      cl2 <- class(vv2) %in% c("factor", "character")
      df <- data.frame(vv1, vv2)
      vars <-  c(input$view_selvar1,input$view_selvar2)
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

  out <- fluidRow(column(12, h4("Analyze existing variables", align="center")))
  rb <- radioButtons("view_rbchoice", choices=c("Plot","Summary"), selected=input$view_rbchoice, label=h5("What should be displayed?"), inline=TRUE, width="100%")

  out <- list(out, fluidRow(
    column(6, uiOutput("ui_selvar1")),
    column(6, uiOutput("ui_selvar2"))))

  out <- list(out, fluidRow(
    column(12, plotOutput("view_plot", height="500px"))
  ))
  out <- list(out, uiOutput("view_summary"))
  out
})

output$ui_reset_var <- renderUI({
  if (is.null(obj$inputdata)) {
    return(NULL)
  }

  sel_reset <- selectInput("sel_reset_microvars", label=h5("Choose variable(s) to reset"),
    choices=names(obj$inputdata), multiple=TRUE, selected=input$sel_reset_microvars)

  if (is.null(input$sel_reset_microvars) || length(input$sel_reset_microvars)==0) {
    btn_reset <- NULL
  } else {
    btn_reset <- myActionButton("btn_resetmicrovar",label="Reset selected variable(s) to their original state", "primary", css.class="btn-xs")
  }
  list(
    fluidRow(
      column(12, h4("Reset variables"), align="center"),
      column(12, p("In this screen you can reset variable(s) in the imported microdata set. That means any prior modification steps will be undone."), align="center"),
      column(12, sel_reset, align="center"),
      column(12, btn_reset, align="center")
    ))
})

# UI-output to display and reset currently available microdata
output$ui_show_microdata <- renderUI({
  my_data_dt = reactive({
    datatable(inputdata(),
    rownames = FALSE,
    selection="none",
    options = list(scrollX=TRUE, lengthMenu=list(c(10, 25, 100, -1), c('10', '20', '100', 'All')), pageLength=25))
  })
  #, options = list(scrollX=TRUE, lengthMenu=list(c(10, 25, 100, -1), c('10', '20', '100', 'All')), pageLength=25), filter="top", rownames=FALSE
  output$tab_inputdata <- DT::renderDataTable({
    my_data_dt()
  })

  output$btn_inputdata <- renderUI({
    invalidateLater(15000)
    if (obj$reset_inputdata1>0) {
      # show real reset button!
      btn <- myActionButton("btn_reset_inputdata",label=("By clicking, you really delete the current inputdata"), "danger", css.class="btn-xs")
    } else {
      btn <- myActionButton("btn_reset_inputdata1",label=("Reset inputdata"), "warning", css.class="btn-xs")
    }
    fluidRow(column(12, p(btn, align="center")))
  })

  out <- fluidRow(
    column(12, h4(paste("Microdata in use:",shQuote(obj$microfilename))), align="center"),
    column(12, p("The dataset has",code(nrow(obj$inputdata)),"observations in",code(ncol(obj$inputdata)),"variables and can be used to set up the",code("sdcMicroObj"),
      "that can be anonymized."), align="center"))
  out <- list(out, uiOutput("btn_inputdata"))
  out <- list(out, fluidRow(
    column(12, dataTableOutput("tab_inputdata"))))
  return(out)
})

# UI-output to use only a subset of the available microdata
output$ui_sample_microdata <- renderUI({
  sel1 <- selectInput("sel_sdcP_sample_type", label=h5("How would you like to restrict the microdata?"),
  choices=c('n-Percent of the data'='n_perc',
            'the first n-observations'='first_n',
            'every n-th observation'='every_n',
            'exactly n randomly drawn observations'='size_n'),
  selected=input$sel_sdcP_sample_type, multiple=FALSE, width="100%")

  if (!is.null(input$sel_sdcP_sample_type)) {
    sl_val <- 1
    if (input$sel_sdcP_sample_type=="n_perc") {
      sl_from <- 1
      sl_to <- 100
    }
    if (input$sel_sdcP_sample_type=="first_n") {
      sl_from <- 1
      sl_to <- nrow(obj$inputdata)
    }
    if (input$sel_sdcP_sample_type=="every_n") {
      sl_from <- 1
      sl_to <- max(pmin(1:nrow(obj$inputdata), 500))
    }
    if (input$sel_sdcP_sample_type=="size_n") {
      sl_from <- 1
      sl_to <- nrow(obj$inputdata)
    }
    sl1 <- sliderInput("sel_sdcP_sample_n", label=h5("Choose your 'n'"),
      value=sl_val, min=sl_from, max=sl_to, step=1, width="100%")
  } else {
    sl1 <- NULL
  }

  btn <- myActionButton("btn_sample_microdata", label="Apply subsetting of the microdata", btn.style="primary")

  out <- fluidRow(
    column(12, h4("Use only a subset of the dataset", align="center")),
    column(12, p("You can restrict the microdataset that is used in the following anonymization procedure. This
      will reduce computation time of the algorithms", align="center")))
  out <- list(out, fluidRow(
    column(6, p(sel1, align="center")),
    column(6, p(sl1, align="center"))))
  out <- list(out, fluidRow(column(12, p(btn, align="center"))))
  out
})

output$ui_modify_data_main <- renderUI({
  out <- NULL
  if (!is.null(input$sel_moddata)) {
    if (input$sel_moddata=="show_microdata") {
      out <- list(out, uiOutput("ui_show_microdata"))
    }
    if (input$sel_moddata=="sample_microdata") {
      out <- list(out, uiOutput("ui_sample_microdata"))
    }
    if (input$sel_moddata=="view_var") {
      out <- list(out, uiOutput("ui_view_var"))
    }
    if (input$sel_moddata=="reset_var") {
      out <- list(out, uiOutput("ui_reset_var"))
    }
    if (input$sel_moddata=="set_to_na") {
      out <- list(out, uiOutput("ui_set_to_na"))
    }
    if (input$sel_moddata=="recode_to_factor") {
      out <- list(out, uiOutput("ui_modify_recode_to_factor"))
    }
    if (input$sel_moddata=="recode_to_numeric") {
      out <- list(out, uiOutput("ui_modify_recode_to_numeric"))
    }
    if (input$sel_moddata=="modify_factor") {
      out <- list(out, uiOutput("ui_modify_change_factor"))
    }
    if (input$sel_moddata=="createstratvar") {
      out <- list(out, uiOutput("ui_modify_create_stratvar"))
    }
  }
  out
})

output$ui_modify_data_sidebar_left <- renderUI({
  choices_modifications <- reactive({
    cc <- c(
      "Display/Reset Microdata"="show_microdata",
      "Explore variables"="view_var",
      "Reset variables"="reset_var",
      "Use only a subset of the available microdata"="sample_microdata",
      "Convert numeric variables to factors"="recode_to_factor",
      "Convert variables to numeric"="recode_to_numeric",
      "Modify an existing factor-variable"="modify_factor",
      "Create a stratification variable"="createstratvar",
      "Set specific values in a variable to NA"="set_to_na")
    if (!is.null(sdcObj())) {
      cc <- cc[1:2]
    }
    return(cc)
  })

  rb <- radioButtons("sel_moddata", label=h5("What do you want to do?"),
    choices=choices_modifications(),
    selected=input$sel_moddata, width="100%")
  fluidRow(
    column(12, p(rb, align="center"))
  )
})

output$ui_modify_data <- renderUI({
  fluidRow(
    column(3, uiOutput("ui_modify_data_sidebar_left")),
    column(9, uiOutput("ui_modify_data_main")))
})
