output$ui_rescat_riskinfo <- renderUI({
  # rb for measure-selection
  output$rb_riskselection <- renderUI({
    radioButtons("rb_riskselection", label=h5("What kind of results do you want to show?"),
      choices=c("Risk Measures"="ui_rescat_riskymeasures", "Risky Observations"="ui_rescat_riskyobs", "Plot of risks"="ui_rescat_riskplot"),
      selected=input$rb_riskselection, inline=TRUE, width="100%")
  })

  # Risk-measures
  output$ui_rescat_riskymeasures <- renderUI({
    riskinfo <- measure_riskComp()

    out <- fluidRow(
      column(12, h5("Risk-measures", align="center")),
      column(12, p(code(riskinfo$s),"observations (",code(riskinfo$sorig),"in the original data) have a
      higher risk than the benchmark value of", code(riskinfo$benchmark),".", align="center")),
      column(12, p("We expect",code(riskinfo$exp_reident_m),"re-identifications (",code(riskinfo$exp_reident_mp),"%) in the
        anonymized data set. In the original dataset we expected",code(riskinfo$exp_reident_o),"(",code(riskinfo$exp_reident_op),"%) re-identifications.", align="center")))

    if (riskinfo$hierrisk) {
      out <- list(out, fluidRow(
        column(12, p("If",strong("hierarchical information"),"is taken into account, we expect to have",code(riskinfo$hier_exp_m),
          "(",code(riskinfo$hier_exp_mp),"%) re-identifications given the anonymized data set. For the original data, we expected to have",
          code(riskinfo$hier_exp_o),"(",code(riskinfo$hier_exp_op),"%)) re-identifications.", align="center"))))
    }
    out
  })

  # table and slider observation with risk > than specified threshold
  output$ui_rescat_riskyobs <- renderUI({
    # slider for minimal risk
    output$riskyobs_slider <- renderUI({
      sliderInput("sl_riskyobs", label=p("Minimum Risk for observations to be shown in the Table below"), min=0, max=max(get_risk()$risk), value=0, width="100%")
    })

    # table containing the corresponding observations
    output$tab_risk <- renderDataTable({
      if (is.null(sdcObj())) {
        return(NULL)
      }
      if (is.null(input$sl_riskyobs)) {
        return(NULL)
      }
      df <- as.data.frame(get_origData()[,get_keyVars()])
      rk <- get_risk()
      df$fk <- rk$fk
      df$Fk <- rk$Fk
      df$indivRisk=rk$risk
      df[!duplicated(df),]
      df[order(df$indivRisk, decreasing=TRUE),]
      df[df$indivRisk > input$sl_riskyobs,,drop=F]
    }, options = list(pageLength = 10, searching=FALSE))

    fluidRow(
      column(12, h5("Display risky-observations in a Table", align="center")),
      column(12, uiOutput("riskyobs_slider")),
      column(12, dataTableOutput("tab_risk")))
  })

  # display a risk-plot
  output$ui_rescat_riskplot <- renderUI({
    output$plot_risk <- renderPlot({
      if (is.null(sdcObj())) {
        return(NULL)
      }
      rk <- get_risk()
      hist(rk$risk, xlab="Risks", main="Individual risks", col="lightgrey")
    })
    fluidRow(
      column(12, h5("Plot showing distribution of individual reidentification-risks", align="center")),
      column(12, plotOutput("plot_risk")))
  })

  out <- fluidRow(
    column(12, h4("Information on Risks", align="center")),
    column(12, div(uiOutput("rb_riskselection"), align="center")))
  if (!is.null(input$rb_riskselection)) {
    if (input$rb_riskselection=="ui_rescat_riskymeasures") {
      out <- list(out, uiOutput("ui_rescat_riskymeasures"))
    }
    if (input$rb_riskselection=="ui_rescat_riskyobs") {
      out <- list(out, uiOutput("ui_rescat_riskyobs"))
    }
    if (input$rb_riskselection=="ui_rescat_riskplot") {
      out <- list(out, uiOutput("ui_rescat_riskplot"))
    }
  }
  out
})

# display information on recodings
output$ui_rescat_recodes <- renderUI({
  # calculate recoding information-loss measures
  output$tab_recodes <- DT::renderDataTable({
    if (is.null(sdcObj())) {
      return(NULL)
    }

    # key var | nr_cat_orig | nr_cat_mod | mean.size(orig) | smallest(orig)
    kV_o <- get_origData()[,get_keyVars_names(), drop=F]
    kV_m <- get_manipKeyVars()

    res_o <- lapply(kV_o, function(x) { c(nr=length(unique(x)), mean=mean(table(x)), min=min(table(x)))})
    res_m <- lapply(kV_m, function(x) { c(nr=length(unique(x)), mean=mean(table(x)), min=min(table(x)))})
    k <- ncol(kV_o)
    out <- NULL
    for (i in 1:k) {
      out <- rbind(out,
        data.frame(keyVar=names(kV_o[i]),
          nrCategories.orig=res_o[[i]][1], nrCategories.mod=res_m[[i]][1],
          mean.size.orig=res_o[[i]][2], mean.size.mod=res_m[[i]][2],
          min.size.orig=res_o[[i]][3], min.size.mod=res_m[[i]][3]))
    }
    out
  }, rownames=FALSE, options = list(scrollX=TRUE, pageLength = 10, searching=FALSE))

  fluidRow(
    column(12, h4("Display information loss based on Recodings of categorical key variables", align="center")),
    column(12, p("For each categorical key variable, the following key figures are computed:", align="center")),
    column(12, p("a) The number of categories in original and modified variables, b) The mean size
    of groups in original and modified variables and c) The size of the smallest category/group in original and modified variables.", align="center")),
    column(12, dataTableOutput("tab_recodes")))
})

# display information on l-diversity risk-measure
output$ui_rescat_ldiv <- renderUI({
  # recursive constant
  output$ldiv_recconst <- renderUI({
    val <- input$ldiv_recconst
    if (is.null(val)) {
      val <- 2
    }
    sliderInput("ldiv_recconst", label=h5("Select a value for the recursive-constant"), min=1, max=100, value=val, width="100%")
  })
  # sensitive variable
  output$ldiv_sensvar <- renderUI({
    vv <- setdiff(allVars(), c(get_weightVar_name(),get_keyVars_names()))
    selectInput("ldiv_sensvar", label=h5("Select one or more sensitive-variables"), choices=vv, multiple=TRUE, selected=input$ldiv_sensvar, width="100%")
  })
  # button
  output$ldiv_btn <- renderUI({
    if (is.null(input$ldiv_sensvar) || length(input$ldiv_sensvar)==0) {
      return(NULL)
    }
    myActionButton("btn_ldiv", label="Calculate l-diversity risk-measure", btn.style="primary")
  })
  # ldiversity-results
  output$ldiv_result <- renderPrint({
    if (is.null(input$ldiv_sensvar) || length(input$ldiv_sensvar)==0) {
      return(NULL)
    }
    curObj <- sdcObj()
    res <- curObj@risk$ldiversity
    if (is.null(res)) {
      return(NULL)
    }
    print(res)
  })
  #  data table showing violating obs
  output$ldiv_violating <- renderDataTable({
    curObj <- sdcObj()
    risk <- curObj@risk
    ldiv <- risk$ldiversity
    if (is.null(input$ldiv_sensvar) || length(input$ldiv_sensvar)==0 || is.null(ldiv)) {
      return(NULL)
    }
    ldiv <- ldiv[,grep("_Distinct_Ldiversity",colnames(ldiv)),drop=FALSE]
    fk <- risk$individual[,2]
    TFfk <- apply(ldiv,1,function(x)any(x<input$ldiv_recconst))
    if (!any(TFfk)) {
      return(data.frame())
    }
    orig <- get_origData()
    kV <- get_manipKeyVars()
    nV <- get_manipNumVars()
    orig <- orig[,!colnames(orig) %in% c(colnames(kV), colnames(nV)), drop=FALSE]
    d <- orig
    if (!is.null(kV)) {
      d <- cbind(kV, orig)
    }
    if (!is.null(nV))
      d <- cbind(nV,orig)
    xtmp <- cbind(ldiv[TFfk,],fk[TFfk],d[TFfk,])
    colnames(xtmp)[1:ncol(ldiv)] <- colnames(ldiv)
    colnames(xtmp)[ncol(ldiv)+1] <- "fk"
    xtmp <- xtmp[order(xtmp[,1]),]
    xtmp
  }, options=list(scrollX=TRUE))

  res <- fluidRow(column(12, h4("l-Diversity risk-measure", align="center")))
  res <- list(res, fluidRow(
    column(6, uiOutput("ldiv_sensvar")),
    column(6, uiOutput("ldiv_recconst"))
  ))
  res <- list(res, fluidRow(
    column(12, uiOutput("ldiv_btn")),
    column(12, verbatimTextOutput("ldiv_result")),
    column(12, dataTableOutput("ldiv_violating"))))
})

# display suda2-risk measure
output$ui_rescat_suda2 <- renderUI({
  # DisFraction
  output$suda2_disf <- renderUI({
    val <- input$suda2_disf
    if (is.null(val)) {
      val <- 0.01
    }
    sliderInput("suda2_disf", label=h5("Select a value for sampling fraction for the stratified sampling"), min=0.01, max=0.5, step=0.01,value=val, width="100%")
  })
  # button
  output$suda2_btn <- renderUI({
    if (is.null(input$suda2_disf)) {
      return(NULL)
    }
    myActionButton("btn_suda2", label="Calculate suda2-scores", btn.style="primary")
  })
  # suda2-results
  output$suda2_result <- renderPrint({
    curObj <- sdcObj()
    res <- curObj@risk$suda2
    if (is.null(input$suda2_disf) || is.null(res)) {
      return(NULL)
    }
    print(res)
  })

  # suda2 can only be calculated for sdcProblems with >= 3 categorical key variables
  if (length(get_keyVars())<=2) {
    return(fluidRow(
      column(12, h4("suda2 risk-measure", align="center")),
      column(12, p("Suda2 scores can only be computed for scenarios with",code(">= 3"),"categorical key variables!", align="center"))
    ))
  }
  return(fluidRow(
    column(12, h4("suda2 risk-measure", align="center")),
    column(12, div(uiOutput("suda2_disf"), align="center")),
    column(12, div(uiOutput("suda2_btn"), align="center")),
    column(12, verbatimTextOutput("suda2_result"))))
})

# information on k-anonymity
output$ui_rescat_violating_kanon <- renderUI({
  output$violating_obs_tab <- renderDataTable({
    risks <- get_risk()
    ii <- which(risks$fk<input$k_val_violating)
    if (length(ii)==0) {
      return(NULL)
    }
    df <- cbind(get_origData()[ii, get_keyVars()], risks[ii,])
    df[order(df$fk),]
  })

  output$ui_kanon_selection <- renderUI({
    if ( is.null(input$k_val_violating)) {
      val <- 3
    } else {
      val <- input$k_val_violating
    }
    sl <- sliderInput("k_val_violating",label=h5("Select value for 'k'"), value=val, min=1, max=50, step=1, width="100%")
    fluidRow(column(12, div(sl, align="center")))
  })

  output$ui_kanon_result <- renderUI({
    if (is.null(input$k_val_violating)) {
      return(NULL)
    }
    fk <- get_risk()$fk
    n1 <- sum(fk < input$k_val_violating)
    n2 <- paste0("(",formatC(100*(n1/length(fk)), format="f", digits=3),"%)")
    res <- h4(code(n1),"observations", code(n2),"violate ",input$k_val_violating,"-anonymity")
    fluidRow(column(12, div(res, align="center")))
  })

  out <- list(
    fluidRow(column(12, h4("Observe observations violating k-anonymity", align="center"))),
    uiOutput("ui_kanon_selection"),
    uiOutput("ui_kanon_result"),
    fluidRow(column(12, dataTableOutput("violating_obs_tab"))))
  out
})

output$ui_catvar1 <- renderUI({
  kv <- get_keyVars_names()
  if (length(kv)>1) {
    kv <- setdiff(get_keyVars_names(), input$sel_catvar2)
  }
  if (is.null(input$sel_catvar1)) {
    sel <- kv[1]
  } else {
    sel <- input$sel_catvar1
  }
  selectInput("sel_catvar1",label="Variable 1", choices=kv, selected=sel, width="100%")
})
output$ui_catvar2 <- renderUI({
  kv <- c("none",setdiff(get_keyVars_names(), input$sel_catvar1))
  if ( length(kv)==0) {
    return(NULL)
  }
  if (is.null(input$sel_catvar2)) {
    sel <- "none"
  } else {
    if ( input$sel_catvar2==input$sel_catvar1) {
      sel <- "none"
    } else {
      sel <- input$sel_catvar2
    }
  }
  selectInput("sel_catvar2",label="Variable 2", choices=kv, selected=sel, width="100%")
})

# mosaicplot of one or two categorical key-variables
output$ui_rescat_mosaicplot <- renderUI({
  output$ui_mosaic_selection <- renderUI({
    fluidRow(
      column(6, uiOutput("ui_catvar1"), align="center"),
      column(6, uiOutput("ui_catvar2"), align="center"))
  })
  output$mosaicplot_o <- renderPlot({
    req(input$sel_catvar1)
    df <- get_origData()
    vars <- c(input$sel_catvar1, input$sel_catvar2)
    if (input$sel_catvar2=="none") {
      barplot(table(df[[vars[1]]]))
    } else {
      n <- length(unique(df[[vars[1]]]))
      cols <- colorRampPalette(c("#DADFE1", "#1E824C"), alpha=TRUE)(n)
      mosaicplot(as.formula(paste("~",paste(vars,collapse="+"),sep="")), data=df, main="", color=cols)
    }
  })
  output$mosaicplot_m <- renderPlot({
    req(input$sel_catvar1)
    df <- get_manipKeyVars()
    vars <- c(input$sel_catvar1, input$sel_catvar2)
    if (input$sel_catvar2=="none") {
      barplot(table(df[[vars[1]]]))
    } else {
      n <- length(unique(df[[vars[1]]]))
      cols <- colorRampPalette(c("#DADFE1", "#1E824C"), alpha=TRUE)(n)
      mosaicplot(as.formula(paste("~",paste(vars,collapse="+"),sep="")), data=df, main="", color=cols)
    }
  })
  out <- list(
    fluidRow(column(12, h4("Graphical representation of original and modified data"), align="center")),
    uiOutput("ui_mosaic_selection"),
    fluidRow(column(12, h4("Original data"), align="center")),
    fluidRow(column(12, plotOutput("mosaicplot_o", height="600px"))),
    fluidRow(column(12, h4("Modified data"), align="center")),
    fluidRow(column(12, plotOutput("mosaicplot_m", height="600px"))))
  out
})

# bivariate tabulation of (modified) key variables
output$ui_bivariate_tab <- renderUI({
  output$ui_biv_selection <- renderUI({
    fluidRow(
      column(6, uiOutput("ui_catvar1"), align="center"),
      column(6, uiOutput("ui_catvar2"), align="center"))
  })
  output$biv_tab_o <- renderTable({
    req(input$sel_catvar1)
    df <- get_origData()
    vars <- c(input$sel_catvar1, input$sel_catvar2)
    if (vars[2]=="none") {
      tab <- addmargins(table(df[[vars[1]]], useNA="always"))
    } else {
      tab <- addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA="always"))
    }
    tab <- as.data.frame.table(tab)
    tab$Freq <- as.integer(tab$Freq)
    if (ncol(tab)==2) {
      colnames(tab) <- c(vars[1], "Freq")
    } else {
      colnames(tab) <- c(vars, "Freq")
    }
    tab
  })
  output$biv_tab_m <- renderTable({
    req(input$sel_catvar1)
    df <- get_manipKeyVars()
    vars <- c(input$sel_catvar1, input$sel_catvar2)
    if (vars[2]=="none") {
      tab <- addmargins(table(df[[vars[1]]], useNA="always"))
    } else {
      tab <- addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA="always"))
    }
    tab <- as.data.frame.table(tab)
    tab$Freq <- as.integer(tab$Freq)
    if (ncol(tab)==2) {
      colnames(tab) <- c(vars[1], "Freq")
    } else {
      colnames(tab) <- c(vars, "Freq")
    }
    tab
  })
  return(list(
    fluidRow(column(12, h4("Tabular representation of original and modified data"), align="center")),
    uiOutput("ui_biv_selection"),
    fluidRow(column(12, h4("Original data"), align="center")),
    fluidRow(column(12, div(tableOutput("biv_tab_o"), align="center"))),
    fluidRow(column(12, h4("Modified data"), align="center")),
    fluidRow(column(12, div(tableOutput("biv_tab_m"), align="center")))))
})
