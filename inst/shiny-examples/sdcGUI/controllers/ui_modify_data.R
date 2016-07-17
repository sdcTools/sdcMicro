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
      out <- list(
        htmlTemplate("tpl_one_col.html", inp=h5("Breaks")),
        htmlTemplate("tpl_one_col.html", inp=helpText(help_sl)),
        htmlTemplate("tpl_one_col.html", inp=sl1))
      out
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
      out <- list(
        htmlTemplate("tpl_one_col.html", inp=h5("Breaks")),
        htmlTemplate("tpl_one_col.html", inp=helpText(help_br)),
        htmlTemplate("tpl_one_col.html", inp=sel_br))
      out
    })
  })

  vv <- numVars()
  if ( length(vv) == 0) {
    return(htmlTemplate("tpl_one_col.html", inp=h4("No numeric variables available in the inputdata!")))
  }

  out <- htmlTemplate("tpl_one_col.html", inp=h4("Recode to Factor"))

  # list all numVars
  btn_rec <- NULL
  sel1 <- selectInput("sel_num",label=h5("Choose numeric variable"), choices=vv, selected=input$sel_num, width="100%")
  help_sel1 <- "Which variable do you want to convert into a factor-variable?"
  sel2 <- radioButtons("sel_custom_split",label=h5("Custom Breaks"), choices=c("no","yes"),
    selected=input$sel_custom_split, inline=TRUE, width="100%")
  help_sel2 <- "If you choose yes, you will have to choose custom break-points"

  out <- list(out,
    htmlTemplate("tpl_two_col.html", inp1=helpText(help_sel1), inp2=helpText(help_sel2)),
    htmlTemplate("tpl_two_col.html", inp1=sel1, inp2=sel2)
  )
  cutalgo <- selectInput("sel_algo",label=h5("Select Algorithm"),
    choices=c("equidistant","logEqui","equalAmount","manual"), selected=input$sel_algo, width="100%")
  help_cutalgo <- list("You need to specify manual breakpoints!")

  if (!is.null(input$sel_custom_split) && input$sel_custom_split=="yes") {
    out <- list(out, htmlTemplate("tpl_three_col.html", inp1=NULL, inp2=cutalgo, inp3=NULL))
  }

  btn_rec <- myActionButton("btn_recode_to_factor",label=("Recode to factor"), "primary")
  isolate({
    if (!is.null(input$sel_custom_split) && input$sel_custom_split=="yes") {
      if (!is.null(input$sel_algo) && input$sel_algo!="manual") {
        out <- list(out, uiOutput("ui_globalRecode_auto"))
      } else {
        out <- list(out, uiOutput("ui_globalRecode_custom"))
      }
      if ((is.null(input$txt_custom_breaks) || input$txt_custom_breaks=="") & is.null(input$sl_number_breaks)) {
        btn_rec <- NULL
      }
    }
    txt_lab <- "Labels are automatically generated, you can change them later once the factor has been generated!"
    out <- list(out,
      htmlTemplate("tpl_one_col.html", inp=h5("Note about factor-labels")),
      htmlTemplate("tpl_one_col.html", inp=helpText(txt_lab)),
      htmlTemplate("tpl_one_col.html", inp=btn_rec))
  })
  out
})

# UI-output for modifying a factor-variable
output$ui_modify_change_factor <- renderUI({
  # plot of curretn factor
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
  out <- htmlTemplate("tpl_one_col.html", inp=h4("Modify a Factor"))
  if ( length(vv) > 0 ) {
    selfac1 <- selectInput("sel_factor",label=NULL,
      choices=vv, selected=input$sel_factor, width="100%")
    cbgr <- checkboxGroupInput("cbg_factor",label=NULL, inline=FALSE,
      choices=curFactorVals(), selected=input$cbg_factor, width="100%")
    if ( is.null(input$cbg_factor) ) {
      btnUp <- NULL
      txtval <- NULL
    } else {
      btnUp <- myActionButton("btn_update_factor",label="Update factor", "primary")
      txtval <- textInput("inp_newlevname",label=NULL,
      value=paste0(input$cbg_factor, collapse="_"), width="100%")
    }
    out <- list(out,
      htmlTemplate("tpl_three_col.html", inp1=h5("Choose factor variable"), inp2=h5("Levels"), inp3=h5("new Levelcode")),
      htmlTemplate("tpl_three_col.html", inp1=selfac1, inp2=cbgr, inp3=txtval),
      htmlTemplate("tpl_one_col.html", inp=btnUp),
      htmlTemplate("tpl_one_col.html", inp=plotOutput("plot_fac")))
  } else {
    return(noInputData())
  }
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

  list(
    htmlTemplate("tpl_one_col.html", inp=h4("Create stratification variable")),
    htmlTemplate("tpl_one_col.html", inp=helpText("Choose at least one variable. The values of the selected variables are chained together and you can use the new variable will be added to the dataset.")),
    htmlTemplate("tpl_two_col.html", inp1=sel, inp2=txtval),
    htmlTemplate("tpl_one_col.html", inp=btn)
  )
})

# UI-output to create a stratification variable
output$ui_set_to_na <- renderUI({
  output$tab_inputdata_setna <- renderDataTable({
    a <- obj$inputdata
    cbind(id=1:nrow(a),a)
  }, options = list(scrollX=TRUE, lengthMenu=list(c(5, 15, 50, -1), c('5', '15', '50', 'All')), pageLength=10))
  output$ui_nasuppid <- ui_custom_numericInput(id="num_na_suppid", label="In which ID do you want to suppress values?", min=1, max=nrow(get_origData()))
  output$ui_nasuppvar <- ui_custom_selectInput(choices=allVars(), id="sel_na_suppvar", multiple=TRUE, label="Select Variable for Suppression")
  output$ui_ansuppbtn <- renderUI({
    if (is.null(input$sel_na_suppvar) || length(input$sel_na_suppvar)==0) {
      return(NULL)
    }
    if (is.null(input$num_na_suppid) || length(input$num_na_suppid)==0) {
      return(NULL)
    }
    myActionButton("btn_set_to_na",label=("Set values to NA"), "primary")
  })
  list(
    htmlTemplate("tpl_two_col.html", inp1=uiOutput("ui_nasuppid"),inp2=uiOutput("ui_nasuppvar")),
    htmlTemplate("tpl_one_col.html", inp=uiOutput("ui_ansuppbtn")),
    htmlTemplate("tpl_one_col.html", inp=dataTableOutput("tab_inputdata_setna")))
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
      return(myActionButton("btn_topbotcoding",label=("Apply Top/Bottom-Coding"), "primary"))
    }
  })

  list(
    htmlTemplate("tpl_one_col.html", inp=plotOutput("ui_topbot_plot")),
    htmlTemplate("tpl_two_col.html", inp1=uiOutput("ui_topbotvar"),inp2=uiOutput("ui_topbotkind")),
    htmlTemplate("tpl_two_col.html", inp1=uiOutput("ui_topbotval"), inp2=uiOutput("ui_topbot_replacement")),
    htmlTemplate("tpl_one_col.html", inp=uiOutput("ui_topbot_btn")))
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
        out <- list(tab=table(df[[1]], df[[2]], useNA="always"), var=c(input$view_selvar1,input$view_selvar2))
        dimnames(out$tab)[[1]][length(dimnames(out$tab)[[1]])] <- "NA"
        dimnames(out$tab)[[2]][length(dimnames(out$tab)[[2]])] <- "NA"
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
    out <- htmlTemplate("tpl_one_col.html", inp=h4("The following Error has occured!\n"))
    out <- list(out, htmlTemplate("tpl_one_col.html", inp=code(lastError())))
    return(out)
  }

  out <- NULL
  if (is.null(obj$sdcObj)) {
    btn <- myActionButton("btn_resetmicrovar",label=paste("Reset",input$view_selvar,"to original state"), "primary")
    out <- list(out, htmlTemplate("tpl_one_col.html", inp=btn))
  }
  rb <- radioButtons("view_rbchoice", choices=c("Plot","Summary"), selected=input$view_rbchoice, label=h4("What should be displayed?"), inline=TRUE, width="100%")
  out <- list(out, htmlTemplate("tpl_three_col.html", inp1=uiOutput("ui_selvar1"), inp2=uiOutput("ui_selvar2"), inp3=rb))

  if (!is.null(input$view_rbchoice)) {
    if (input$view_rbchoice=="Plot") {
      out <- list(out, htmlTemplate("tpl_one_col.html", inp=plotOutput("view_plot", height="500px")))
    }
    if (input$view_rbchoice=="Summary") {
      res_stats <- stats_summary()
      if (is.null(res_stats$tab1)) {
        out <- list(out, htmlTemplate("tpl_one_col.html", inp=renderTable(res_stats$tab, include.rownames=TRUE)))
      } else {
        out <- list(out, htmlTemplate("tpl_one_col.html", inp=h5(HTML(paste("Correlation between",code(res_stats$vars[1]),"and",code(res_stats$vars[2]),":",code(res_stats$vcor))))))
        out <- list(out, htmlTemplate("tpl_one_col.html", inp=h5(HTML(paste("Summary of Variable",code(res_stats$vars[1]))))))
        out <- list(out, htmlTemplate("tpl_one_col.html", inp=renderTable(res_stats$tab1, include.rownames=FALSE)))
        out <- list(out, htmlTemplate("tpl_one_col.html", inp=h5(HTML(paste("Summary of Variable",code(res_stats$vars[2]))))))
        out <- list(out, htmlTemplate("tpl_one_col.html", inp=renderTable(res_stats$tab2, include.rownames=FALSE)))
      }
    }
  }
  out
})

# UI-output to allow to undo all modifications
#output$ui_reset_vars <- renderUI({
#})

# UI-output to display and reset currently available microdata
output$ui_show_microdata <- renderUI({
  output$tab_inputdata <- renderDataTable({
    obj$inputdata
  }, options = list(scrollX=TRUE, engthMenu=list(c(5, 15, 50, -1), c('5', '15', '50', 'All')), pageLength=10))
  btn <- myActionButton("btn_reset_inputdata",label=("Reset inputdata"), "danger")
  out <- list(
    htmlTemplate("tpl_one_col.html", inp=h4("Current Microdata")),
    htmlTemplate("tpl_one_col.html", inp=helpText("These data may be used to set up the sdcObj which can be anonymized!")),
    htmlTemplate("tpl_one_col.html", inp=btn),
    htmlTemplate("tpl_one_col.html", inp=dataTableOutput("tab_inputdata")))
  out
})

output$ui_modify_data <- renderUI({
  out <- htmlTemplate("tpl_one_col.html",inp=h2("Modify/Recode Data"))

  if ( is.null(obj$inputdata) ) {
    return(list(out, noInputData()))
  }

  choices_modifications <- reactive({
    cc <- c(
      "Display/Reset Microdata"="show_microdata",
      "Explore variables"="view_var",
      "Convert numeric variables to factors"="recode_to_factor",
      "Modify an existing factor-variable"="modify_factor",
      "Create a stratification variable"="createstratvar",
      "Set specific values in a variable to NA"="set_to_na",
      "Apply Top-/Bottom Coding"="topbotcoding")
    if (!is.null(obj$sdcObj)) {
      cc <- cc[1:2]
    }
    return(cc)
  })

  btn <- selectInput("sel_moddata", label=h5("What do you want to do?"),
    #choices=c(
    #  "Display/Reset Microdata"="show_microdata",
    #  "View/Analyse a variable"="view_var",
    #  "Convert numeric variables to factors"="recode_to_factor",
    #  "Modify an existing factor-variable"="modify_factor",
    #  "Create a stratification variable"="createstratvar",
    #  "Set specific values in a variable to NA"="set_to_na",
    #  "Apply Top-/Bottom Coding"="topbotcoding"),
    choices=choices_modifications(),
      selected=input$sel_moddata, width="100%")
  out <- list(out, htmlTemplate("tpl_one_col.html",inp=btn))
  if (!is.null(input$sel_moddata)) {
    if (input$sel_moddata=="show_microdata") {
      out <- list(out, uiOutput("ui_show_microdata"))
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

