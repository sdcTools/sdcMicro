# UI-output for recoding a variable to a factor
output$ui_modify_recode_to_factor <- renderUI({
  # automatically generated breakpoints
  output$ui_globalRecode_auto <- renderUI({
    isolate({
      if (is.null(input$sel_algo)){
        return(NULL)
      }
      sl1 <- sliderInput("sl_number_breaks",label=NULL, value=3, min=2, max=10, step=1, width="100%")
      help_sl <- list("'3' splits the variable into 3 groups. The breaks for",code("cut()"), "are automatically computed.")

      return(fluidRow(
        column(12, h5("Breaks", align="center")),
        column(12, helpText(help_sl)),
        column(12, p(sl1, align="center"))
      ))
    })
  })

  # custom breaks
  output$ui_globalRecode_custom <- renderUI({
    isolate({
      if (is.null(input$sel_algo)){
        return(NULL)
      }
      sel_br <- textInput("txt_custom_breaks",label=NULL, value=input$txt_custom_breaks,width="100%")
      help_br <- list(
        "Example input: 1,3,5,9 splits the variable in 3 groups:", br(),
        "(1,3],(3,5] and (5,9].", br(),
        "If you supply 1 number (e.g. 3), the variable will be split in 3 equal sized groups.")
      return(fluidRow(
        column(12, h5("Breaks", align="center")),
        column(12, helpText(help_br)),
        column(12, p(sel_br, align="center"))
      ))
    })
  })

  vv <- numVars()
  if (length(vv) == 0) {
    return(fluidRow(
      column(12, h4("No numeric variables available in the inputdata!", align="center"))
    ))
  }

  out <- fluidRow(column(12, h4("Recode a numeric variable into a factor", align="center")))

  # list all numVars
  btn_rec <- NULL
  sel1 <- selectInput("sel_num",label=h5("Choose numeric variable"), choices=vv, selected=input$sel_num)
  help_sel1 <- "Which variable do you want to convert into a factor-variable?"
  sel2 <- radioButtons("sel_custom_split",label=h5("Custom Breaks"), choices=c("no","yes"),
    selected=input$sel_custom_split, inline=TRUE, width="100%")
  help_sel2 <- "If you choose yes, you will have to choose custom break-points"

  out <- list(out, fluidRow(
    column(6, helpText(help_sel1)),
    column(6, helpText(help_sel2))))
  out <- list(out, fluidRow(
    column(6, p(sel1, align="center")),
    column(6, p(sel2, align="center"))))

  cutalgo <- selectInput("sel_algo",label=h5("Select Algorithm"),
    choices=c("equidistant","logEqui","equalAmount","manual"), selected=input$sel_algo, width="100%")
  help_cutalgo <- list("You need to specify manual breakpoints!")

  if (!is.null(input$sel_custom_split) && input$sel_custom_split=="yes") {
    out <- list(out, fluidRow(
      column(3, p(cutalgo, align="center")),
      column(6, "")))
  }

  btn_rec <- myActionButton("btn_recode_to_factor",label=("Recode to factor"), "primary")
  isolate({
    if (!is.null(input$sel_custom_split) && input$sel_custom_split=="yes") {
      if (!is.null(input$sel_algo) && input$sel_algo!="manual") {
        out <- list(out, uiOutput("ui_globalRecode_auto"))
      } else {
        out <- list(out, uiOutput("ui_globalRecode_custom"))
        print(input$txt_custom_breaks)
        if ((is.null(input$txt_custom_breaks) || input$txt_custom_breaks=="")) {
          btn_rec <- NULL
        }
      }
    }
    txt_lab <- "Labels are automatically generated, you can change them later once the factor has been generated!"
    out <- list(out, fluidRow(
      column(12, h5("Note about factor-labels"), align="center"),
      column(12, helpText(txt_lab), align="center"),
      column(12, btn_rec, align="center")))
  })
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
  cbgr <- checkboxGroupInput("cbg_factor",label=NULL, inline=FALSE,
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
    column(4, h5("Levels", align="center")),
    column(4, h5("new Levelcode", align="center"))))

  out <- list(out, fluidRow(
    column(4, p(selfac1, align="center")),
    column(4, p(cbgr, align="center")),
    column(4, p(txtval, align="center"))))

  out <- list(out, fluidRow(
    column(12, p(btnUp, align="center")),
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

# UI-output for top/bottom-coding of numerical variables
output$ui_topbotcoding <- renderUI({
  output$ui_topbot_plot <- renderPlot({
    if (is.null(input$sel_topbot_var) ) {
      return(NULL)
    }
    vv <- obj$inputdata[[input$sel_topbot_var]]
    boxplot(obj$inputdata[[input$sel_topbot_var]], main=input$sel_topbot_var, xlab=input$sel_topbot_var, col="#DADFE1")
  })

  output$ui_topbotvar <- ui_custom_selectInput(choices=numVars(), id="sel_topbot_var", multiple=FALSE, label="Select variable")
  output$ui_topbotkind <- ui_custom_selectInput(choices=c("top","bottom"), id="sel_topbot_kind", multiple=FALSE, label="Apply Top- or Bottom-Coding?")
  output$ui_topbotval <- ui_custom_textInput(id="num_topbot_val", label="Value", placeholder="Please enter a number")
  output$ui_topbot_replacement <- ui_custom_textInput(id="num_topbot_replacement", label="Replacement Value", placeholder="Please enter a number")

  output$ui_topbot_btn <- renderUI({
    num1 <- suppressWarnings(as.numeric(input$num_topbot_val))
    num2 <- suppressWarnings(as.numeric(input$num_topbot_replacement))
    if (is.null(input$sel_topbot_var)) {
      return(NULL)
    }
    if (is.na(num1) || is.na(num2)) {
      return(NULL)
    }
    if (is.null(num1) || is.null(num2)) {
      return(NULL)
    }
    if (is.numeric(num1) & is.numeric(num2)) {
      if (input$sel_topbot_kind=="top") {
        n <- sum(obj$inputdata[[input$sel_topbot_var]] >= num1)
      } else {
        n <- sum(obj$inputdata[[input$sel_topbot_var]] <= num1)
      }
      return(fluidRow(
        column(12, p("A total of",code(n),"values will be replaced!"), align="center"),
        column(12, myActionButton("btn_topbotcoding",label=("Apply Top/Bottom-Coding"), "primary"), align="center")
      ))
    } else {
      return(NULL)
    }
  })

  out <- fluidRow(
    column(12, h4("Apply Top- or Bottom coding", align="center")),
    column(12, plotOutput("ui_topbot_plot")))

  out <- list(out, fluidRow(
    column(6, uiOutput("ui_topbotvar")),
    column(6, uiOutput("ui_topbotkind"))))

  out <- list(out, fluidRow(
    column(6, uiOutput("ui_topbotval")),
    column(6, uiOutput("ui_topbot_replacement"))))

  out <- list(out, uiOutput("ui_topbot_btn"))
  out
})

# UI-output to display a variable
# users can choose a summary or a plot which depends
# on the class of the variable
output$ui_selvar1 <- renderUI({
  allV <- allVars()
  ii <- which(allV==input$view_selvar2)
  if (length(ii)==1) {
    allV <- allV[-c(ii)]
  }
  selectInput("view_selvar1", choices=allV, selected=input$view_selvar1,
    label=h5("Choose a variable"), multiple=FALSE, width="100%")
})
output$ui_selvar2 <- renderUI({
  if (is.null(input$view_selvar1)) {
    return(NULL)
  }
  allV <- allVars()
  ii <- which(allV==input$view_selvar1)
  if (length(ii)==1) {
    allV <- allV[-c(ii)]
  }
  cc <- c("none",allV)
  selectInput("view_selvar2", choices=cc, selected=input$view_selvar2,
    label=h5("Choose a second variable (optional)"), multiple=FALSE, width="100%")
})

output$ui_view_var <- renderUI({
  stats_summary <- reactive({
    if (is.null(input$view_selvar1) ) {
      return(NULL)
    }

    if (!is.null(input$view_selvar2) && input$view_selvar2!="none") {
      df <- data.frame(obj$inputdata[[input$view_selvar1]], obj$inputdata[[input$view_selvar2]])
      colnames(df) <- c(input$view_selvar1,input$view_selvar2)
      cl1 <- class(df[[1]]) %in% c("factor", "character")
      cl2 <- class(df[[2]]) %in% c("factor", "character")
    } else {
      df <- data.frame(obj$inputdata[[input$view_selvar1]])
      colnames(df) <- input$view_selvar1
      cl1 <- class(df[[1]]) %in% c("factor", "character")
    }

    if (is.null(input$view_selvar2) || input$view_selvar2=="none") {
      if (cl1) {
        out <- list(tab=summaryfn(obj$inputdata[[input$view_selvar1]]))
        colnames(out$tab) <- c(input$view_selvar1, "Frequency")
      } else {
        out <- list(tab=as.data.frame(t(summaryfn(obj$inputdata[[input$view_selvar1]]))))
      }
    } else {
      # 2 factors
      if (cl1 & cl2) {
        out <- list(tab=as.data.frame.table(addmargins(table(df[[1]], df[[2]], useNA="always"))), var=c(input$view_selvar1,input$view_selvar2))
        colnames(out$tab) <- c(out$var, "Frequency")
      } else if (cl1 & !cl2) {
        out <- tapply(df[[2]], df[[1]], summaryfn)
        out <- do.call("rbind", out)
        bb <- data.frame(f=rownames(out))
        colnames(bb) <- input$view_selvar1
        out <- cbind(bb, out)
        rownames(out) <- NULL
        out <- list(tab=out)
      } else if (!cl1 & cl2) {
        out <- tapply(df[[1]], df[[2]], summaryfn)
        out <- do.call("rbind", out)
        bb <- data.frame(f=rownames(out))
        colnames(bb) <- input$view_selvar2
        out <- cbind(bb, out)
        rownames(out) <- NULL
        out <- list(tab=out)
      } else {
        # two numeric variables
        tab1 <- as.data.frame(t(summaryfn(df[[1]])))
        tab2 <- as.data.frame(t(summaryfn(df[[2]])))
        vcor <- round(cor(df[[1]], df[[2]]),3)
        out <- list(vars=c(input$view_selvar1,input$view_selvar2),tab1=tab1, tab2=tab2, vcor=vcor)
      }
    }
    out
  })

  output$view_plot <- renderPlot({
    if (is.null(input$view_selvar1) ) {
      return(NULL)
    }
    if (is.null(input$view_selvar2) ) {
      return(NULL)
    }

    vv1 <- obj$inputdata[[input$view_selvar1]]
    if (input$view_selvar2=="none") {
      if (is.factor(vv1) | is.character(vv1)) {
        tt <- table(vv1, useNA="always")
        names(tt)[length(tt)] <- "NA"
        barplot(tt, col="#DADFE1")
      } else {
        hist(vv1, main=NULL, xlab=input$view_selvar1, col="#DADFE1")
      }
    } else {
      vv2 <- obj$inputdata[[input$view_selvar2]]
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
  if (is.null(sdcObj())) {
    btn <- myActionButton("btn_resetmicrovar",label=paste("Reset",input$view_selvar1,"to original state"), "primary", css.class="btn-xs")
    out <- list(out, fluidRow(column(12, p(btn, align="center"))))
  }
  rb <- radioButtons("view_rbchoice", choices=c("Plot","Summary"), selected=input$view_rbchoice, label=h5("What should be displayed?"), inline=TRUE, width="100%")
  out <- list(out, fluidRow(
    column(4, uiOutput("ui_selvar1")),
    column(4, uiOutput("ui_selvar2")),
    column(4, p(rb, align="center"))))

  if (!is.null(input$view_rbchoice)) {
    if (input$view_rbchoice=="Plot") {
      out <- list(out, fluidRow(column(12, plotOutput("view_plot", height="500px"))))
    }
    if (input$view_rbchoice=="Summary") {
      res_stats <- stats_summary()
      if (is.null(res_stats$tab1)) {
        out <- list(out, fluidRow(column(12, renderTable(res_stats$tab, include.rownames=FALSE), align="center")))
      } else {
        out <- list(out, fluidRow(
          column(12, h5(HTML(paste("Correlation between",code(res_stats$vars[1]),"and",code(res_stats$vars[2]),":",code(res_stats$vcor))), align="center")),
          column(12, h5(HTML(paste("Summary of Variable",code(res_stats$vars[1]))), align="center")),
          column(12, renderTable(res_stats$tab1, include.rownames=FALSE), align="center"),
          column(12, h5(HTML(paste("Summary of Variable",code(res_stats$vars[2]))), align="center")),
          column(12, renderTable(res_stats$tab2, include.rownames=FALSE), align="center")))
      }
    }
  }
  out
})

# UI-output to display and reset currently available microdata
output$ui_show_microdata <- renderUI({
  output$tab_inputdata <- renderDataTable({
    obj$inputdata
  }, options = list(scrollX=TRUE, engthMenu=list(c(5, 15, 50, -1), c('5', '15', '50', 'All')), pageLength=10))
  if (obj$reset_inputdata1>0) {
    # show real reset button!
    btn <- myActionButton("btn_reset_inputdata",label=("By clicking, you really delete the current inputdata"), "danger", css.class="btn-xs")
  } else {
    btn <- myActionButton("btn_reset_inputdata1",label=("Reset inputdata"), "warning", css.class="btn-xs")
  }

  return(fluidRow(
    column(12, h4(paste("Microdata in use:",shQuote(obj$microfilename))), align="center"),
    column(12, p("The dataset has",code(nrow(obj$inputdata)),"observations in",code(ncol(obj$inputdata)),"variables and can be used to set up the",code("sdcMicroObj"),
      "that can be anonymized."), align="center"),
    column(12, p(btn, align="center")),
    column(12, dataTableOutput("tab_inputdata"))))
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
    if (input$sel_moddata=="set_to_na") {
      out <- list(out, uiOutput("ui_set_to_na"))
    }
    if (input$sel_moddata=="recode_to_factor") {
      out <- list(out, uiOutput("ui_modify_recode_to_factor"))
    }
    if (input$sel_moddata=="modify_factor") {
      out <- list(out, uiOutput("ui_modify_change_factor"))
    }
    if (input$sel_moddata=="createstratvar") {
      out <- list(out, uiOutput("ui_modify_create_stratvar"))
    }
    if (input$sel_moddata=="topbotcoding") {
      out <- list(out, uiOutput("ui_topbotcoding"))
    }
  }
  out
})

output$ui_modify_data_sidebar_left <- renderUI({
  choices_modifications <- reactive({
    cc <- c(
      "Display/Reset Microdata"="show_microdata",
      "Explore/Reset variables"="view_var",
      "Use only a subset of the available microdata"="sample_microdata",
      "Convert numeric variables to factors"="recode_to_factor",
      "Modify an existing factor-variable"="modify_factor",
      "Create a stratification variable"="createstratvar",
      "Set specific values in a variable to NA"="set_to_na",
      "Apply Top-/Bottom Coding"="topbotcoding")
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
