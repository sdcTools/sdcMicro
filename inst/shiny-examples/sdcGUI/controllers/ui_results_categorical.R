# display a table with observations ordered by descending individual risk
output$ui_rescat_riskyobs <- renderUI({
  # slider for minimal risk
  output$riskyobs_slider <- renderUI({
    sliderInput("sl_riskyobs", label=h5("Minimum Risk for observations to be shown in the Table below"), min=0, max=max(get_risk()$risk), value=0, width="100%")
  })

  # table containing the corresponding observations
  output$tab_risk <- renderDataTable({
    if (is.null(obj$sdcObj)) {
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

  return(list(
    htmlTemplate("tpl_one_col.html",inp=h4("Display risky-observations in a Table")),
    htmlTemplate("tpl_one_col.html",inp=uiOutput("riskyobs_slider")),
    htmlTemplate("tpl_one_col.html",inp=dataTableOutput("tab_risk"))))
})

# display information on recodings
output$ui_rescat_recodes <- renderUI({
  # calculate recoding information-loss measures
  output$tab_recodes <- renderDataTable({
    if (is.null(obj$sdcObj)) {
      return(NULL)
    }

    # key var | nr_cat_orig | nr_cat_mod | mean.size(orig) | smallest(orig)
    kV_o <- get_origData()[,get_keyVars_names()]
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
  }, options = list(pageLength = 10, searching=FALSE))

  return(list(
    htmlTemplate("tpl_one_col.html",inp=h4("Display information loss based on Recodings of categorical key variables")),
    htmlTemplate("tpl_one_col.html", inp=p("For each categorical key variable, the following key figures are computed:")),
    htmlTemplate("tpl_one_col.html", inp=p("a) The number of categories in original and modified variables, b) The mean size
      of groups in original and modified variables and c) The size of the smallest category/group in original and modified variables")),
    htmlTemplate("tpl_one_col.html",inp=dataTableOutput("tab_recodes"))))
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
    res <- obj$sdcObj@risk$ldiversity
    if (is.null(res)) {
      return(NULL)
    }
    print(res)
  })
  #  data table showing violating obs
  output$ldiv_violating <- renderDataTable({
    risk <- obj$sdcObj@risk
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
  })

  res <- list(
    htmlTemplate("tpl_one_col.html",inp=h4("l-Diversity risk-measure")),
    htmlTemplate("tpl_two_col.html",inp1=uiOutput("ldiv_sensvar"), inp2=uiOutput("ldiv_recconst")),
    htmlTemplate("tpl_one_col.html",inp=uiOutput("ldiv_btn")),
    htmlTemplate("tpl_one_col.html",inp=verbatimTextOutput("ldiv_result")),
    htmlTemplate("tpl_one_col.html",inp=dataTableOutput("ldiv_violating")))
  res
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
    res <- obj$sdcObj@risk$suda2
    if (is.null(input$suda2_disf) || is.null(res)) {
      return(NULL)
    }
    print(res)
  })

  # suda2 can only be calculated for sdcProblems with >= 3 categorical key variables
  if (length(get_keyVars()<=2)) {
    return(list(
      htmlTemplate("tpl_one_col.html",inp=h4("suda2 risk-measure")),
      htmlTemplate("tpl_one_col.html",inp=p("Suda2 scores can only be computed for scenarios with",code(">= 3"),"categorical key variables!"))
    ))
  }
  return(list(
    htmlTemplate("tpl_one_col.html",inp=h4("suda2 risk-measure")),
    htmlTemplate("tpl_two_col.html",inp1=uiOutput("suda2_disf"), inp2=uiOutput("suda2_btn")),
    htmlTemplate("tpl_one_col.html",inp=verbatimTextOutput("suda2_result"))
  ))
})

# display a risk-plot
output$ui_rescat_freqCalc <- renderUI({
  output$plot_risk <- renderPlot({
    if (is.null(obj$sdcObj)) {
      return(NULL)
    }
    rk <- get_risk()
    hist(rk$risk, xlab="Risks", main="Individual risks", col="lightgrey")
  })
  return(list(
    htmlTemplate("tpl_one_col.html",inp=h4("Plot of showing individual reidentification-risks")),
    htmlTemplate("tpl_one_col.html",inp=plotOutput("plot_risk"))))
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
    sl <- sliderInput("k_val_violating",label=h4("Select value for 'k'"), value=val, min=1, max=50, step=1, width="100%")
    htmlTemplate("tpl_three_col.html",inp1=NULL, inp2=sl, inp3=NULL)
  })

  output$ui_kanon_result <- renderUI({
    if (is.null(input$k_val_violating)) {
      return(NULL)
    }
    fk <- get_risk()$fk
    n1 <- sum(fk < input$k_val_violating)
    n2 <- paste0("(",formatC(100*(n1/length(fk)), format="f", digits=3),"%)")
    res <- h4(code(n1),"observations", code(n2),"violate ",input$k_val_violating,"-anonymity")
    htmlTemplate("tpl_one_col.html", inp=res)
  })

  out <- list(
    uiOutput("ui_kanon_selection"),
    uiOutput("ui_kanon_result"),
    htmlTemplate("tpl_one_col.html", inp=dataTableOutput("violating_obs_tab")))
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
output$ui_show_orig_or_modified <- renderUI({
  cc <- c("original data"="orig", "modified data"="modified")
  radioButtons("rb_orig_or_modified",label=h5("What do you want to use?"),
    choices=cc, selected=input$rb_orig_or_modified, width="100%", inline=TRUE)
})

# mosaicplot of one or two categorical key-variables
output$ui_rescat_mosaicplot <- renderUI({
  output$ui_mosaic_selection <- renderUI({
    out <- htmlTemplate("tpl_three_col.html",
      inp1=uiOutput("ui_catvar1"), inp2=uiOutput("ui_catvar2"), inp3=uiOutput("ui_show_orig_or_modified"))
    out
  })
  output$mosaicplot <- renderPlot({
    if (is.null(input$sel_catvar1)) {
      return(NULL)
    }
    if (is.null(input$rb_orig_or_modified)) {
      return(NULL)
    }
    if (input$rb_orig_or_modified=="orig") {
      df <- get_origData()
    } else {
      df <- get_manipKeyVars()
    }
    vars <- c(input$sel_catvar1, input$sel_catvar2)

    if (input$sel_catvar2=="none") {
      barplot(table(df[[vars[1]]]))
    } else {
      n <- length(unique(df[[vars[1]]]))
      cols <- colorRampPalette(c("#DADFE1", "#1E824C"), alpha=TRUE)(n)
      mosaicplot(as.formula(paste("~",paste(vars,collapse="+"),sep="")),data=df,main="", color=cols)
    }
  })
  out <- list(
    uiOutput("ui_mosaic_selection"),
    uiOutput("ui_mosaic_result"),
    htmlTemplate("tpl_one_col.html", inp=plotOutput("mosaicplot", height="600px")))
  out
})

# bivariate tabulation of (modified) key variables
output$ui_bivariate_tab <- renderUI({
  output$ui_biv_selection <- renderUI({
    out <- htmlTemplate("tpl_three_col.html",
      inp1=uiOutput("ui_catvar1"), inp2=uiOutput("ui_catvar2"), inp3=uiOutput("ui_show_orig_or_modified"))
    out
  })
  output$biv_tab <- renderTable({
    if (is.null(input$sel_catvar1)) {
      return(NULL)
    }
    if (is.null(input$rb_orig_or_modified)) {
      return(NULL)
    }
    if (input$rb_orig_or_modified=="orig") {
      df <- get_origData()
    } else {
      df <- get_manipKeyVars()
    }
    vars <- c(input$sel_catvar1, input$sel_catvar2)
    if (vars[2]=="none") {
      tab <- addmargins(table(df[[vars[1]]]))
    } else {
      tab <- addmargins(table(df[[vars[1]]], df[[vars[2]]]))
    }
  })
  out <- list(
    uiOutput("ui_biv_selection"),
    htmlTemplate("tpl_three_col.html", inp1=NULL, inp2=tableOutput("biv_tab"), inp3=NULL))
  out
})

