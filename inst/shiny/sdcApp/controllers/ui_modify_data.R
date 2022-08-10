# UI-output for recoding a variable to a numeric variable
output$ui_modify_recode_to_numeric <- renderUI({
  output$ui_to_num_var <- renderUI({
    selectInput("sel_to_num_var",label=p("Choose variable(s)"), choices=vv, width="50%", multiple=TRUE)
  })
  output$ui_to_num_btn <- renderUI({
    req(input$sel_to_num_var)
    if (length(input$sel_to_num_var)>0) {
      return(myActionButton("btn_recode_to_numeric",label=("Recode to numeric"), "primary"))
    }
  })
  vv <- c(facVars(), charVars())
  if (length(vv) == 0) {
    return(fluidRow(
      column(12, h4("No factor variables available in the inputdata!", align="center")),
      column(12, h4("There are no variables in the input data that could be converted to numeric variables.", align="center"))
    ))
  }

  helptxt <- "Numerical key variables have to be of type 'numeric' or type 'integer'. Variables of type 'character' or type 'factor' need to be converted"
  helptxt <- paste(helptxt, "to type 'numeric' before selecting these as numerical key variables. Here you can convert variables of type 'character' and")
  helptxt <- paste(helptxt, "type 'factor' to type 'numeric'.")
  out <- fluidRow(
    column(width = 12, offset = 0, h3("Convert character/factor variables to numeric variables"), class="wb-header"),
    column(width = 12, offset = 0, p(helptxt), class="wb-header-hint"),
    column(12, uiOutput("ui_to_num_var"), align="center"),
    column(12, uiOutput("ui_to_num_btn"), align="center"))
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
    return(numericInput("sl_number_breaks",label=p("Specify number of intervals"), value=3, min=2, max=20, step=1))
  })
  output$ui_globalRecode_manual <- renderUI({
    if (is.null(input$sel_algo)){
      return(NULL)
    }
    sel_br <- customTextInput("txt_custom_breaks",label=p("Specify custom breaks"), value=input$txt_custom_breaks)
    help_br <- helpText(
      "Example input: 1,3,5,9 splits the variable into the 3 groups (1,3],(3,5] and (5,9].", br(),
      "If you supply 1 number (e.g. 3), the variable will be split in 3 equally sized groups.")
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
    vv <- numVars()
    mult <- FALSE
    if (!is.null(input$sel_custom_split) && input$sel_custom_split=="no") {
      selectInput("sel_num_glrec", label=p("Choose numeric variable(s)"), choices=vv, multiple=TRUE)
    } else {
      radioButtons("rb_num_glrec", label=p("Choose a numeric variable"), choices=vv, selected=input$rb_num_glrec)
    }
  })
  output$ui_globalRecode_split <- renderUI({
    radioButtons("sel_custom_split",label=p("Use custom breaks?"), choices=c("no","yes"),
      selected=input$sel_custom_split, inline=TRUE, width="100%")
  })
  output$ui_globalRecode_cutalgo <- renderUI({
    txt <- paste("<strong>Description of algorithms:</strong><br /><br />")
    txt <- paste(txt, "- <strong>equidistant:</strong> uses breakpoints that generate intervals of equal length. The number of records in each interval might differ.<br />")
    txt <- paste(txt, "- <strong>logEqui:</strong> uses breakpoints that generate intervals of equal length based on the log transformation of the data. The number of records in each interval might differ.<br />")
    txt <- paste(txt, "- <strong>equalAmount:</strong> uses breakpoints such that each group/interval has the same number of records. The intervals might be of different length.<br />")
    txt <- paste(txt, "- <strong>manual:</strong> allows the user to set the breakpoints manually. Note: make sure that all values are included in the specified intervals.")
    selectInput("sel_algo",label=p("Select algorithm", tipify(icon("circle-info"), title=txt, placement="top")),
      choices=c("equidistant","logEqui","equalAmount","manual"), selected=input$sel_algo)
  })
  output$ui_globalRecode_btn <- renderUI({
    req(input$sel_custom_split)
    btn_rec <- myActionButton("btn_recode_to_factor", label=("Convert to factor"), "primary")
    if (input$sel_custom_split=="no") {
      if (length(input$sel_num_glrec)==0) {
        return(NULL)
      } else {
        return(btn_rec)
      }
    } else {
      btn_rec <- myActionButton("btn_recode_to_factor", label=("Convert to factor"), "primary")
      if (!is.null(input$sel_algo) && input$sel_algo!="manual") {
        if (is.null(input$sl_number_breaks)) {
          return(NULL)
        } else {
          if (is.na(input$sl_number_breaks)) {
            return(NULL)
          } else if (input$sl_number_breaks<1) {
            return(NULL)
          } else {
            return(btn_rec)
          }
        }
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
        } else if (length(res)==1 && res > nrow(obj$inputdata)) {
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

  helptxt <- "Categorical key variables have to be of type 'factor' or type 'integer'.  Variables of type 'numeric' need to be converted to type 'factor'"
  helptxt <- paste(helptxt,"selecting these as categorical key variables. Here you can convert variables of type 'numeric' to type 'factor'.")
  helptxt <- paste(helptxt, "Each unique value is converted to a separate category (factor level). In case several values in the numeric variable should")
  helptxt <- paste(helptxt, "be combined into one factor level, customized breaks can be specified. Several algorithms are available to customize the breaks.")

  out <- fluidRow(
    column(width = 12, offset = 0, h3("Convert numeric to factor"), class="wb-header"),
    column(width = 12, offset = 0, p(helptxt), class="wb-header-hint")
  )

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
  # current factor-levels
  curFactorVals <- reactive({
    inp <- inputdata()
    if (is.null(inp)) {
      return(NULL)
    }
    if (is.null(input$sel_factor) ) {
      return(NULL)
    }
    ff <- inp[[input$sel_factor]]
    ll <- as.list(levels(ff))
    # add output for debugging, remove later
    if (length(ll)==0 | length(table(ff))==0) {
      cat("problem in variable",dQuote(input$sel_factor)," (length(levels(x)) or length(table(x))) is 0!\n")
      return(NULL)
    }
    if (length(ll) != length(table(ff))) {
      cat("problem in variable",dQuote(input$sel_factor)," (length(levels(x)) != length(table(x)))!\n")
      return(NULL)
    }
    names(ll) <- paste0(ll, " (",table(ff)," obs)")
    ll
  })
  output$plot_fac <- renderPlot({
    if (is.null(input$sel_factor)) {
      return(NULL)
    }
    df <- table(inputdata()[[input$sel_factor]], useNA="always")
    dn <- dimnames(df)[[1]]
    dn[length(dn)] <- "NA"
    dimnames(df)[[1]] <- dn
    mp <- barplot(df, las=1, xaxt = 'n')
    mtext(gsub(paste0("(.{", round(120/length(dn)), "})"), paste0("\\1", "\n"), dn), side = 1, line = 2, at = mp)
  })
  output$reclocfac_var <- renderUI({
    vv <- facVars()
    selectInput("sel_factor", label=p("Choose factor variable"), choices=vv, selected=input$sel_factor, width="50%")
  })
  output$reclocfac_levs <- renderUI({
    selectInput("cbg_factor", label=p("Select Levels to recode/combine"),
      multiple=TRUE, selectize=TRUE, choices=curFactorVals(), width="100%")
  })
  output$reclocfac_addna <- renderUI({
    req(input$cbg_factor)
    radioButtons("rb_micro_addna", h5("Add missing values to new factor level?"), choices=c("no", "yes"), inline=TRUE)
  })
  output$reclocfac_btn <- renderUI({
    req(input$cbg_factor)
    myActionButton("btn_update_factor", label="Group factor levels", "primary")
  })
  output$reclocfac_txtval <- renderUI({
    req(input$cbg_factor)
    txtval <- textInput("inp_newlevname", label=p("New label for recoded values"),
      value=paste0(input$cbg_factor, collapse="_"), width="100%")
  })

  out <- fluidRow(
     column(12, h3("Group factor levels in factor variables in original data"), class="wb-header")
  )

  if (length(facVars())==0) {
    out <- list(out, fluidRow(
      column(12, h5("There are no factor variables available that could be recoded!", align="center"))
    ))
    return(out)
  }

  helptxt <- p("Here you can group/combine the factor levels of categorical variables of type 'factor' before setting up the sdcMicro object.")
  out <- list(out, fluidRow(
    column(12, helptxt, class="wb-header-hint")))
  out <- list(out, fluidRow(
    column(12, uiOutput("reclocfac_var"), align="center")))
  out <- list(out, fluidRow(
    column(4, uiOutput("reclocfac_levs"), align="center"),
    column(4, uiOutput("reclocfac_txtval"), align="center"),
    column(4, uiOutput("reclocfac_addna"), align="center")
    ))

  out <- list(out, fluidRow(
    column(12, uiOutput("reclocfac_btn"), align="center"),
    column(12, plotOutput("plot_fac"))))
  out
})

# UI-output to create a stratification variable
output$ui_modify_create_stratvar <- renderUI({
  output$sel_genstrata <- renderUI({
    selectInput("sel_allvars_strata",label=p("Select variables to generate a stratification variable"), multiple=TRUE,
      choices=allVars(), width="100%")
  })
  output$vname_genstrata <- renderUI({
    req(input$sel_allvars_strata)
    if (length(input$sel_allvars_strata) < 2) {
      return(NULL)
    }
    textInput("inp_vname_strata", label=p("Specify variable name for stratification variable"), value=paste0(input$sel_allvars_strata, collapse="_"), width="100%")
  })
  output$btn_genstrata <- renderUI({
    req(input$sel_allvars_strata, input$inp_vname_strata)
    if (length(input$sel_allvars_strata) < 2) {
      return(NULL)
    }
    if (input$inp_vname_strata %in% colnames(inputdata())) {
      return(NULL)
    }
    fluidRow(column(12, myActionButton("btn_create_stratavar", label=("Create stratification variable"), "primary"), align="center"))
  })

  helptxt <- "Many SDC methods can be applied within strata. Here you can generate a new stratification variable by chaining together values of two or more"
  helptxt <- paste(helptxt, "variables. The number of strata is the product of the number of factor levels in the selected variables. For instance by choosing")
  helptxt <- paste(helptxt, "gender (male, female) and region (region 1, region 2), 4 strata are generated (male - region 1, male - region 2, female - region 1, female - region 2).")
  helptxt <- paste(helptxt, "By default the variable name of the stratification variable consists of the variable names separated by  '_' . You can also specify")
  helptxt <- paste(helptxt, "the variable name by typing it into the text field. The new variable is added to the loaded micro data set and will be exported.")
  out <- fluidRow(
    column(width = 12, offset = 0, h3("Create a stratification variable"), class="wb-header"),
    column(width = 12, offset = 0, p(helptxt), class="wb-header-hint")
  )

  out <- list(out, fluidRow(
    column(6, uiOutput("sel_genstrata"), align="center"),
    column(6, uiOutput("vname_genstrata"), align="center")))
  out <- list(out, uiOutput("btn_genstrata"))
  out
})

# UI-output to set specific values to NA
output$ui_set_to_na <- renderUI({
  output$ui_nasupptype <- renderUI({
    radioButtons("set_to_na_type", label=p("How to select values to recode to NA?"),
      choices=c("by value"="rule", "by record ID"="id"), inline=TRUE)
  })
  output$tab_inputdata_setna <- DT::renderDataTable({
    a <- obj$inputdata
    cbind(id=1:nrow(a),a)
  },
    options=list(scrollX=TRUE, scrollY=250, searching=FALSE, paging=TRUE, ordering=FALSE, bInfo=FALSE))
  output$ui_nasuppvar <- renderUI({
    req(input$set_to_na_type)
    multiple <- FALSE
    if (input$set_to_na_type=="id") {
      multiple <- TRUE
    }
    res <- selectInput("sel_na_suppvar", choices=allVars(), multiple=multiple, label=p("Select variable"), width="50%")
    return(res)
  })
  output$ui_nasuppid <- renderUI({
    req(input$sel_na_suppvar, input$set_to_na_type)
    if (input$set_to_na_type=="id") {
      res <- numericInput("num_na_suppid", label=p("Select record ID"), value=1, min=1, max=nrow(obj$inputdata))
    } else {
      res <- selectInput("num_na_suppid", label=p("Select value to recode to NA"), multiple=FALSE, choices=sort(unique(obj$inputdata[[input$sel_na_suppvar]])))
    }
    return(res)
  })
  output$ui_ansuppbtn <- renderUI({
    req(input$sel_na_suppvar, input$num_na_suppid, input$set_to_na_type)
    btn <- myActionButton("btn_set_to_na",label=("Set values to NA"), "primary")
    if (input$set_to_na_type=="rule") {
      return(btn)
    }
    if (length(input$sel_na_suppvar)==0){
      return(NULL)
    }
    if (length(input$num_na_suppid)==0) {
      return(NULL)
    }
    if (input$num_na_suppid<1) {
      return(NULL)
    }
    if (input$num_na_suppid>nrow(inputdata())) {
      return(NULL)
    }
    return(btn)
  })

  helptxt <- paste0("The loaded dataset may contain different missing value codes, such as",code(9),",",code(999),",",code(-9),", etc. ")
  helptxt <- paste0(helptxt, "sdcMicro can only interpret missing values that are coded",code("NA"),". Here you can set other missing value")
  helptxt <- paste0(helptxt, "codes to",code("NA"),"(by missing value code). All records with that value in the variable will be recoded to NA. Alternatively one can set individual cells to",code("NA"),"by selecting the variable and record id (by record ID).")
  helptxt <- paste0(helptxt, " Note that it impossible to later retrieve the original missing values. R does not allow for distinct missing value codes.")

  out <- fluidRow(
    column(width = 12, offset = 0, h3("Set missing values to NA"), class="wb-header"),
    column(width = 12, offset = 0, p(HTML(helptxt)), class="wb-header-hint"),
    column(12, uiOutput("ui_nasupptype"), align="center")
  )

  out <- list(out, fluidRow(
    column(6, uiOutput("ui_nasuppvar"), align="center"),
    column(6, uiOutput("ui_nasuppid"), align="center")))
  out <- list(out, fluidRow(
    column(12, uiOutput("ui_ansuppbtn"), align="center"),
    column(12, DT::dataTableOutput("tab_inputdata_setna"), align="center")))
  out
})

# UI-output to display a variable
# users can choose a summary or a plot which depends
# on the class of the variable
output$ui_view_var <- renderUI({
  output$ui_selvar1 <- renderUI({
    selectInput("view_selvar1", choices=allVars(), label=p("Choose a variable"), multiple=FALSE, selected=obj$inp_sel_viewvar1, width="100%")
  })
  # This is required so that usual changes of the dropdown-select are also reflected in the reactive variable obj$inp_sel_viewvar1
  observeEvent(input$view_selvar1, {
    obj$inp_sel_viewvar1 <- input$view_selvar1
  })
  output$ui_selvar2 <- renderUI({
    vv <- setdiff(allVars(), input$inp_sel_viewvar1)
    selectInput("view_selvar2", choices=c("none", vv), label=p("Choose a second variable (optional)"), multiple=FALSE, width="100%")
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
    if (input$view_selvar2!="none") {
      ii <- which(input$view_selvar2==vv)
      if (length(ii)>0) {
        vv <- vv[-c(ii)]
      }
    }
    updateSelectInput(session, inputId="view_selvar1", choices=vv, selected=input$view_selvar1)
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

    if (!is.null(v2) && v2 != "none") {
      df <- data.frame(inputdata[[v1]], inputdata[[v2]])
      colnames(df) <- c(v1, v2)
      cl1 <- class(df[[1]]) %in% c("factor", "character")
      cl2 <- class(df[[2]]) %in% c("factor", "character")
    } else {
      df <- data.frame(inputdata[[v1]])
      colnames(df) <- v1
      cl1 <- class(df[[1]]) %in% c("factor", "character")
    }
    if (!is.null(v2) && v2 == "none") {
      if (cl1) {
        res <- list(tab = summaryfn(inputdata[[v1]]))
        colnames(res$tab) <- c(v1, "Frequency", "Percentage")
      } else {
        res <- list(tab = as.data.frame(t(summaryfn(inputdata[[v1]]))))
      }
    } else {
      # two factors
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
        res <- list(vars=c(v1,v2), tab1=tab1, tab2=tab2, vcor=vcor)
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

    nainfo <- data.frame(variable=c(v1, v2))
    nainfo$nr_na <- as.integer(unlist(lapply(df, function(x) { sum(is.na(x)) })))
    nainfo$perc_na <- formatC(100 * (nainfo$nr_na/nrow(df)), format = "f", digits = 2)
    out <- list(out,
      fluidRow(column(12, "Variable",code(nainfo$variable[1]),"has",code(nainfo$nr_na[1]),"(",code(paste0(nainfo$perc_na[1],"%")),") missing values.")))
    if (nrow(nainfo)==2 & nainfo$variable[2]!="none") {
      out <- list(out,
        fluidRow(column(12, "Variable",code(nainfo$variable[2]),"has",code(nainfo$nr_na[2]),"(",code(paste0(nainfo$perc_na[2],"%")),") missing values.")))
    }
    out
  })
  output$view_plot <- renderPlot({
    inputdata <- inputdata()
    if (is.null(inputdata)) {
      return(NULL)
    }
    req(input$view_selvar1, input$view_selvar2)

    vv1 <- inputdata[[input$view_selvar1]]
    if (input$view_selvar2=="none") {
      if (is.factor(vv1) | is.character(vv1)) {
        tt <- table(vv1, useNA="always")
        names(tt)[length(tt)] <- "NA"
        #barplot(tt, col="#DADFE1")
        mp <- barplot(tt, col="#DADFE1", las=1, xaxt = 'n')
        mtext(gsub(paste0("(.{", round(120/length(tt)), "})"), paste0("\\1", "\n"), names(tt)), side = 1, line = 2, at = mp)
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
      column(12, h4("The following error has occured!", align="center")),
      column(12, code(lastError()))))
  }

  out <- fluidRow(
    column(12, h3("Explore variables in original data"), class="wb-header")
    )
  rb <- radioButtons("view_rbchoice", choices=c("Plot","Summary"), selected=input$view_rbchoice, label=p("What should be displayed?"), inline=TRUE, width="100%")

  out <- list(out, fluidRow(
    column(width = 12, offset = 0, p("Here you can view tabulations, summary statistics and graphic representations of variables and pairs of variables to explore the original data."), class="wb-header-hint"),
    column(6, uiOutput("ui_selvar1")),
    column(6, uiOutput("ui_selvar2"))))

  out <- list(out, fluidRow(
    column(12, plotOutput("view_plot", height="500px"))
  ))
  out <- list(out, uiOutput("view_summary"))
  out
})

# UI-output to reset a variable to its original state
output$ui_reset_var <- renderUI({
  output$reset_microvar_var <- renderUI({
    selectInput("sel_reset_microvars", label=p("Choose variable(s) to reset"),
      choices=allVars(), multiple=TRUE, width="50%")
  })

  output$reset_microvar_btn <- renderUI({
    req(input$sel_reset_microvars)
    myActionButton("btn_resetmicrovar",label="Reset selected variable(s) to their original state", "primary")
  })

  txt_reset <- "Here you can undo any modifications to variables made after loading the dataset into the GUI."
  txt_reset <- paste(txt_reset, "Resetting restores the variable to its original state and cancels any modifications.")
  list(
    fluidRow(
      column(width = 12, offset = 0, h3("Reset variables"), class="wb-header"),
      column(width = 12, offset = 0, p(txt_reset), class="wb-header-hint"),
      column(12, uiOutput("reset_microvar_var"), align="center"),
      column(12, uiOutput("reset_microvar_btn"), align="center")
    ))
})

# UI-output to display and reset currently available microdata
output$ui_show_microdata <- renderUI({
  my_data_dt = reactive({
    DT::datatable(inputdata(),
              rownames = FALSE,
              selection="none",
              options = list(scrollX=TRUE, scrollY=250, lengthMenu=list(c(20, 50, 100, -1), c('20', '50', '100', 'All')), pageLength=20)
              )
  })
  #, options = list(scrollX=TRUE, lengthMenu=list(c(10, 25, 100, -1), c('10', '20', '100', 'All')), pageLength=25), filter="top", rownames=FALSE
  output$tab_inputdata <- DT::renderDataTable({
    my_data_dt()
  })

  #txt_microdata <- paste0("In this tab you can manipulate the data to prepare for setting up an object of class",code("sdcMicroObj"),"in the Anonymize tab. ")
  txt_microdata <- paste0("The loaded dataset is",code(obj$microfilename),"and consists of",code(nrow(obj$inputdata)),"observations and ",code(ncol(obj$inputdata)),"variables. ")
  if(is.null(attr(obj$inputdata, "dropped"))){
    txt_microdata <- paste0(txt_microdata, "No variables were dropped because of all missing values.")
  }else{
    txt_microdata <- paste0(txt_microdata, code(length(attr(obj$inputdata, "dropped"))), "variable(s) was/were dropped because of all missing values: ")
    #lapply(attr(obj$inputdata, "dropped"), function(x) {txt_microdata <- paste0(txt_microdata, x)})
    #attr(obj$inputdata, "dropped")
  }
  out <- fluidRow(
    column(width = 12, offset = 0, h3("Loaded microdata")), class="wb-header")
  if(is.null(attr(obj$inputdata, "dropped"))){
    out <- list(out, fluidRow(
      column(width = 12, offset = 0, p(HTML(txt_microdata))), class="wb-header-hint"))
  }else{
    out <- list(out, fluidRow(
      column(width = 8, offset = 2, list(HTML(txt_microdata), code(lapply(attr(obj$inputdata, "dropped"), function(x) {x}))))))
  }
  out <- list(out, fluidRow(
    column(12, DT::dataTableOutput("tab_inputdata"))))
  return(out)
})

# UI-output to use only a subset of the available microdata
output$ui_sample_microdata <- renderUI({
  sel1 <- selectInput("sel_sdcP_sample_type", label=p("Select a method to restrict the number of records"),
  choices=c('n percent of the data'='n_perc',
            'the first n observations'='first_n',
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
    sl1 <- sliderInput("sel_sdcP_sample_n", label=p("Set 'n' for the selected method"),
      value=sl_val, min=sl_from, max=sl_to, step=1, width="100%")
  } else {
    sl1 <- NULL
  }

  btn <- myActionButton("btn_sample_microdata", label="Create subset", btn.style="primary")

  txt_subset <- "For testing purposes, you can here reduce the number of records in the dataset by selecting a subset."
  txt_subset <- paste(txt_subset, "This reduces the computation time of several anonymization methods.")
  txt_subset <- paste(txt_subset, "Note: This is solely for testing purposes to reduce the computation time and not an anonymization method. To produce an anonymized dataset this should not be used.")

  out <- fluidRow(
    column(12, h3("Use only a subset of the dataset"), class="wb-header"),
    column(width = 12, offset = 0, p(txt_subset), class="wb-header-hint"))
  out <- list(out, fluidRow(
    column(6, p(sel1, align="center")),
    column(6, p(sl1, align="center"))))
  out <- list(out, fluidRow(column(12, p(btn, align="center"))))
  out
})

# UI-output to deal with hierarchical data (eg. households and individuals)
output$ui_hierarchical_data_prep <- renderUI({
  output$hier_data_prep_btn <- renderUI({
    req(input$sel_hhvars, input$sel_hhvars_id)
    if (input$sel_hhvars_id=="") {
      return(NULL)
    }
    if (length(input$sel_hhvars)>0) {
      return(myActionButton("btn_hier_data_prep", label="Create household-input data", btn.style="primary"))
    }
    return(invisible(NULL))
  })
  output$sel_hhvars_id <- renderUI({
    selectInput("sel_hhvars_id", label=p("Select the household id variable"),
      choices=c("",allVars()), multiple=FALSE, width="75%")
  })
  output$sel_hhvars <- renderUI({
    req(input$sel_hhvars_id)
    selectInput("sel_hhvars", label=p("Please select all variables that refer to households and not to individuals"),
      choices=setdiff(allVars(), input$sel_hhvars_id), multiple=TRUE, width="75%")
  })
  if (obj$hhdata_selected==TRUE) {
    curdat <- inputdata()
    df <- data.frame(
      "variable name"=colnames(curdat),
      "type"=dataTypes())
    return(
      fluidRow(
        column(width = 10, offset = 1, h4("Note"), align="center"),
        column(width = 10, offset = 1, p("The current input data have already been modified to be used as household-level data. The data
          set contains",code(nrow(inputdata())),"observations in the following",code(ncol(inputdata())),"variables."), align="center"),
        column(width = 10, offset = 1, renderTable(df), align="center"),
        column(width = 10, offset = 1, p("If you want to work on individual-level data, you will have to delete the entire microdata file and start from scratch"), align="center")
      ))
  }

  out <- list(fluidRow(column(12, h4("Prepare household-level data"), align="center")))

  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(width = 10, offset = 1, h4("The following Error has occured!", align="center")),
      column(width = 10, offset = 1, code(lastError()), align="center")))
  }

  helptxt <- "One record per household is selected and all variables that do not pertain to the household level are removed. Do not forget to select"
  helptxt <- paste(helptxt, "a variable containing household sampling weights (if available). When setting up an sdcProblem, it is important to use correct sampling weights!")

  out <- list(out, fluidRow(
    column(width = 10, offset = 1, p(helptxt), align="center"),
    column(6, uiOutput("sel_hhvars_id"), align="center"), column(6, uiOutput("sel_hhvars"), align="center"),
    column(width = 10, offset = 1, uiOutput("hier_data_prep_btn"), align="center")
  ))
  out
})

observeEvent(input$sel_hhvars_id, {
  updateSelectInput(session, inputId="sel_hhvars", choices=setdiff(allVars(), input$sel_hhvars_id), selected=setdiff(names(input$sel_hhvars), input$sel_hhvars_id))
})
observeEvent(input$sel_hhvars, {
  updateSelectInput(session, inputId="sel_hhvars_id", choices=allVars(), selected=input$sel_hhvars_id)
})

output$ui_hierarchical_data_merge <- renderUI({
  output$sel_hhid_merge <- renderUI({
    selectInput("sel_hhid_hhdata", label=p("Select a variable containing household ids"), choices=intersect(colnames(obj$hhdata), colnames(inputdata())),
      selected=input$sel_hhid_hhdata)
  })
  output$btn_reset_hhdata <- renderUI({
    myActionButton("reset_hhdata", label="Reset uploaded household-level data", btn.style="danger")
  })
  output$btn_merge_hhdata <- renderUI({
    myActionButton("btn_merge_hhdata", label="Merge household- and individual level data", btn.style="primary")
  })

  if (obj$hhdata_applied==TRUE) {
    curdat <- inputdata()
    df <- data.frame(
      "variable name"=colnames(curdat),
      "type"=dataTypes())
    return(
      fluidRow(
        column(width = 10, offset = 1, h4("Merging of files was successful"), align="center"),
        column(width = 10, offset = 1, p("The (anonymized) household-level data were merged into the input dataset"), align="center"),
        column(width = 10, offset = 1, p("The data set contains",code(nrow(inputdata())),"observations in the following",code(ncol(inputdata())),"variables."), align="center"),
        column(width = 10, offset = 1, renderTable(df), align="center"),
        column(width = 10, offset = 1, p("In order to merge a different household level dataset, it is necessary to remove and reload the loaded microdata."), align="center")
    ))
  }

  if (obj$hhdata_selected==TRUE) {
    return(
      fluidRow(
        column(width = 10, offset = 1, h4("Note"), align="center"),
        column(width = 10, offset = 1, p("The current input data have been modified to be used as household-level data.
          It is therefore not possible, to merge additional household-level data to the current inputdata."), align="center"),
            column(12, p("To do so, you will have to delete the entire microdata file and start from scratch"), align="center")
      ))
  }

  out <- list(fluidRow(column(12, h4("Merge Data"), align="center")))
  if (!is.null(lastError())) {
    out <- list(out, fluidRow(
      column(12, h4("The following error has occured!", align="center")),
      column(12, code(lastError()), align="center")))
  }
  if (!is.null(obj$hhdata)) {
    out <- list(out, fluidRow(
      column(width = 8, offset = 2, p("You can reset the uploaded data by clicking the button below."), align="center"),
      column(12, uiOutput("btn_reset_hhdata"), align="center"),
      column(width = 8, offset = 2, h4("Continue with the merge"), align="center"),
      column(width = 8, offset = 2, p("You can now continue to merge the current household level data to the individual level microdata."), align="center"),
      column(12, uiOutput("sel_hhid_merge"), align="center"),
      column(12, uiOutput("btn_merge_hhdata"), align="center")
    ))
  } else {
    out <- list(out, fluidRow(
      column(width = 8, offset = 2, p("You can now read in an already exported and anonymized household-level file to replace the household level variables in the raw dataset you read in the next step."), align="center"),
      column(width = 8, offset = 2, p("Note: the selected file is loaded immediately. Set options before selecting the file."), align="center"),
      column(12, fileInput("file_hhfile", h5(paste0("Select File (allowed types are '.rdata')")), width="50%", accept=".rdata"), align="center")))
  }
  out
})

output$ui_hierarchical_data <- renderUI({
  rb1 <- radioButtons("rb_hierdata_selection", label=p("What do you want to do?"),
    choices=c("Prepare file for the anonymization of household level variables"="prep_data", "Merge an anonymized household level file into the full dataset"="merge_hhdata"),
    selected=input$rb_hierdata_selection, inline=TRUE)

  helptxt <- "Often microdata files incur a hierarchical structure, such as a household structure. In such cases the anonymization process consists of two steps: "
  helptxt <- paste0(helptxt, tags$br(), "1) the anonymization of the higher level records and variables (household file) and ")
  helptxt <- paste0(helptxt, "2) the anonymization of the complete file with the anonymized higher level file merged into. Here you can create a household ")
  helptxt <- paste0(helptxt, "level file for step 1 and merge the anonymized household level file back into the full dataset for step 2. The dataset needs to contain a household ID.")

  out <- list(
    fluidRow(
      column(width = 12, offset = 0, h3("Deal with hierarchical Data"), class="wb-header"),
      column(width = 12, offset = 0, p(HTML(helptxt)), class="wb-header-hint")
    ),
    fluidRow(column(12, rb1, align="center"))
  )

  if (!is.null(input$rb_hierdata_selection)) {
    if (input$rb_hierdata_selection=="prep_data") {
      out <- list(out, uiOutput("ui_hierarchical_data_prep"))
    }
    if (input$rb_hierdata_selection=="merge_hhdata") {
      out <- list(out, uiOutput("ui_hierarchical_data_merge"))
    }
  }
  out
})

output$ui_modify_data_main <- renderUI({
  out <- NULL
  val <- obj$cur_selection_microdata
  if (val=="btn_menu_microdata_1") {
    return(uiOutput("ui_show_microdata"))
  }
  if (val=="btn_menu_microdata_2") {
    return(uiOutput("ui_view_var"))
  }
  if (val=="btn_menu_microdata_3") {
    return(uiOutput("ui_reset_var"))
  }
  if (val=="btn_menu_microdata_4") {
    return(uiOutput("ui_sample_microdata"))
  }
  if (val=="btn_menu_microdata_5") {
    return(uiOutput("ui_modify_recode_to_factor"))
  }
  if (val=="btn_menu_microdata_6") {
    return(uiOutput("ui_modify_recode_to_numeric"))
  }
  if (val=="btn_menu_microdata_7") {
    return(uiOutput("ui_modify_change_factor"))
  }
  if (val=="btn_menu_microdata_8") {
    return(uiOutput("ui_modify_create_stratvar"))
  }
  if (val=="btn_menu_microdata_9") {
    return(uiOutput("ui_set_to_na"))
  }
  if (val=="btn_menu_microdata_10") {
    return(uiOutput("ui_hierarchical_data"))
  }
  out
})

output$ui_modify_data_sidebar_left <- renderUI({
  output$btn_reset_inputdata <- renderUI({
    if (is.null(inputdata())) {
      return(NULL)
    }
    btn <- myActionButton("btn_reset_inputdata_xx",label=("Reset inputdata"), "danger")
    fluidRow(
      column(12, btn, class="wb-action-button")
    )
  })

  choices_modifications <- reactive({
    cc <- c(
      "Display microdata"="show_microdata",
      "Explore variables"="view_var",
      "Reset variables"="reset_var",
      "Use subset of microdata"="sample_microdata",
      "Convert numeric to factor"="recode_to_factor",
      "Convert variables to numeric"="recode_to_numeric",
      "Modify factor variable"="modify_factor",
      "Create a stratification variable"="createstratvar",
      "Set specific values to NA"="set_to_na",
      "Hierarchical data"="deal_with_hierarchical_data")
    if (!is.null(sdcObj())) {
      cc <- cc[1:2]
    }
    return(cc)
  })

  output$ui_sel_microdata_btns <- renderUI({
    cc <- choices_modifications()
    out <- fluidRow(column(12, h4("What do you want to do?")))
    for (i in 1:length(cc)) {
      id <- paste0("btn_menu_microdata_",i)
      if (obj$cur_selection_microdata==id) {
        style <- "primary"
      } else {
        style <- "default"
      }
      out <- list(out, fluidRow(
        # TODO: See issue https://github.com/skounis/sdcMicro/issues/48
        # column(12, bsButton(id, label=names(cc)[i], block=TRUE, style=style), tags$br())))
        column(12, bsButton(id, label=names(cc)[i], block=TRUE, style=style))))
    }
    # required observers that update the color of the active button!
    eval(parse(text=genObserver_menus(pat="btn_menu_microdata_", n=1:10, updateVal="cur_selection_microdata")))
    out
  })

  fluidRow(
    column(12, uiOutput("ui_sel_microdata_btns")),
    column(12, uiOutput("btn_reset_inputdata"))
  )
})

output$ui_modify_data <- renderUI({
  fluidRow(
    column(2, uiOutput("ui_modify_data_sidebar_left"), class="wb_sidebar"),
    column(10, uiOutput("ui_modify_data_main"), class="wb-maincolumn"))
})
