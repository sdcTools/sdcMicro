# display a table with observations ordered by descending individual risk
output$ui_rescat_riskyobs <- renderUI({
  output$tab_risk <- renderDataTable({
    if ( is.null(obj$sdcObj) ) {
      return(NULL)
    }
    df <- as.data.frame(get_origData()[,get_keyVars()])
    rk <- get_risk()
    df$fk <- rk$fk
    df$Fk <- rk$Fk
    df$indivRisk=rk$risk
    df[!duplicated(df),]
    df[order(df$indivRisk, decreasing=TRUE),]
  }, options = list(pageLength = 10))
  return(htmlTemplate("tpl_one_col.html",inp=dataTableOutput("tab_risk")))
})

# display information on recodings
output$ui_rescat_recodes <- renderUI({
  return(htmlTemplate("tpl_one_col.html",inp=h4("Information about Recodes")))
})

# display a risk-plot
output$ui_rescat_freqCalc <- renderUI({
  output$plot_risk <- renderPlot({
    if ( is.null(obj$sdcObj) ) {
      return(NULL)
    }
    rk <- get_risk()
    hist(rk$risk, xlab="Risks", main="Individual risks", col="lightgrey")
  })
  return(htmlTemplate("tpl_one_col.html",inp=plotOutput("plot_risk")))
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

