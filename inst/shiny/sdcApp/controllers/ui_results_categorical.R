# UI output with info on risk for categorical key variables
output$ui_rescat_riskinfo_header <- renderUI({
  out <- fluidRow(
    column(12, h3("Risk measures"), offset = 0, class = "wb-header"),
    column(12, p("The output on this page is based on the categorical key variables in the current problem."), offset = 0, class = "wb-header-hint"))
  out
})
output$ui_rescat_riskinfo <- renderUI({
  output$ui_rescat_selection <- renderUI({
    radioButtons("rb_riskselection", label=p("What kind of results do you want to show?"),
      choices=c("Risk measures"="rescat_riskymeasures", "Risky observations"="rescat_riskyobs", "Plot of risk"="rescat_riskplot"),
      selected=input$rb_riskselection, inline=TRUE, width="100%")
  })
  output$rescat_riskymeasures <- renderUI({
    rI <- measure_riskComp()
    txt_tooltip <- "These observations have a risk being larger than the median of the risk distribution plus two times "
    txt_tooltip <- paste0(txt_tooltip, "its Median Absolute Deviation (MAD). The individual re-identification risk is computed based on the selected categorical key ")
    txt_tooltip <- paste0(txt_tooltip, "variables,  and reflects both the frequencies of the keys in the data and the individual sampling weights.")
    out <- fluidRow(
      column(12, h5("Risk measures") ),
      column(12, p(code(rI$s),"observations have a higher risk than the risk in the main part of the data, as compared to",code(rI$sorig),"observations in the original data",
        tipify(icon("circle-info"), title=txt_tooltip, placement="top")) ),
      column(12, p("Based on the individual re-identification risk, we expect",code(rI$exp_reident_m),"re-identifications (",code(paste0(rI$exp_reident_mp,"%")),")
      in the anonymized data set. In the original dataset we expected",code(rI$exp_reident_o),"(",code(paste0(rI$exp_reident_op,"%")),") re-identifications.") )
    )
    if (rI$hierrisk) {
      out <- list(out, fluidRow(
        column(12, h5("Expected number of re-identifications taking cluster information into account"), align="center"),
        column(12, p("If cluster information is taken into account, we expect",code(rI$hier_exp_m),
          "(",code(paste0(rI$hier_exp_mp,"%")),") re-identifications in the anonymized data set. In the original dataset we expected",
          code(rI$hier_exp_o),"(",code(paste0(rI$hier_exp_op,"%")),") re-identifications."), align="center")))
    }
    out
  })

  # table and slider observation with risk > than specified threshold
  output$rescat_riskyobs <- renderUI({
    calc_riskyobs <- reactive({
      input$sl_riskyobs
      curObj <- sdcObj()
      if (is.null(curObj)) {
        return(NULL)
      }
      if (is.null(input$sl_riskyobs)) {
        return(NULL)
      }
      df <- get_manipKeyVars()
      N <- nrow(df)
      rk <- get_risk()
      df$fk <- rk$fk
      df$Fk <- rk$Fk
      df$indivRisk <- rk$risk
      df[!duplicated(df),]
      df[order(df$indivRisk, decreasing=TRUE),]
      df <- df[df$indivRisk > input$sl_riskyobs,,drop=F]
      if (nrow(df)>0) {
        df$indivRisk <- formatC(df$indivRisk, format="f", digits=6)
      }
      n <- nrow(df)
      p <- paste0(formatC(100*(n/N), format="f", digits=2),"%")
      return(list(df=df, n=n, p=p))
      df
    })
    # slider for minimal risk
    output$riskyobs_slider <- renderUI({
      sliderInput("sl_riskyobs", label=p("Minimum risk for to be shown in the table"), min=0, max=max(get_risk()$risk), value=0, width="100%")
    })
    output$riskyobs_result <- renderUI({
      # table containing the corresponding observations
      output$tab_risk <- DT::renderDataTable({
        df
      }, options = list(pageLength = 10, searching=FALSE, scrollX=TRUE, scrollY=250))

      res <- calc_riskyobs()
      if (is.null(res)) {
        return(NULL)
      }
      df <- res$df
      n <- res$n
      p <- res$p
      out <- NULL
      if (n == 0) {
        out <- list(out, fluidRow(
          column(12, p(code(n),"(",code(p),") records have a risk larger than",code(input$sl_riskyobs),"."), align="center")
        ))
      } else {
        out <- list(out, fluidRow(
          column(12, p(code(n),"(",code(p),") records have a risk larger than",code(input$sl_riskyobs),"."), align="center"),
          column(12, DT::dataTableOutput("tab_risk"))
        ))
      }
      out
    })


    out <- fluidRow(
      column(12, h5("Display risky observations in a table", align="center")),
      column(12, uiOutput("riskyobs_slider"), align="center"))
    out <- list(out, uiOutput("riskyobs_result"))
    out
  })

  # display a risk-plot
  output$rescat_riskplot <- renderUI({
    output$plot_risk <- renderPlot({
      if (is.null(sdcObj())) {
        return(NULL)
      }
      rk <- get_risk()
      hist(rk$risk, xlab="Risks", main="Individual risks (Anonymized data)", col="lightgrey")
    })

    output$plot_risk_orig <- renderPlot({
      curObj <- sdcObj()
      if (is.null(curObj)) {
        return(NULL)
      }
      rk <- curObj@originalRisk$individual[,1]
      hist(rk, xlab="Risks", main="Individual risks (Original data)", col="lightgrey")
    })

    fluidRow(
      column(12, h5("Plot showing distribution of individual re-identification risk levels", align="center")),
      column(12, h5("Anonymized data"), align="center"),
      column(12, plotOutput("plot_risk"), align="center"),
      column(12, h5("Original data"), align="center"),
      column(12, plotOutput("plot_risk_orig"), align="center"))
  })

  out <- fluidRow(
      column(12, uiOutput("ui_rescat_selection"), align="center"))

  if (!is.null(input$rb_riskselection)) {
    if (input$rb_riskselection=="rescat_riskymeasures") {
      out <- list(out, uiOutput("rescat_riskymeasures"))
    }
    if (input$rb_riskselection=="rescat_riskyobs") {
      out <- list(out, uiOutput("rescat_riskyobs"))
    }
    if (input$rb_riskselection=="rescat_riskplot") {
      out <- list(out, uiOutput("rescat_riskplot"))
    }
  }
  out
})

# UI oputput displaying information on recodings
output$ui_rescat_recodes_header <- renderUI({
  out <- fluidRow(
    column(12, h3("Display information loss based on recodings of categorical key variables"), offset = 0, class = "wb-header"),
    column(12, p("For each categorical key variable, the following key figures are computed:"),
               p("a) The number of categories in original and modified variables."),
               p("b) The mean size of groups in original and modified variables."),
               p("c) The size of the smallest category/group in original and modified variables."),
               offset = 0, class = "wb-header-hint")
    )
  out
})
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
    out$mean.size.orig <- formatC(out$mean.size.orig, format="f", digits=3)
    out$mean.size.mod <- formatC(out$mean.size.mod, format="f", digits=3)
    out
  }, rownames=FALSE, options = list(scrollX=TRUE, pageLength = 10, searching=FALSE))

  fluidRow(
    column(12, DT::dataTableOutput("tab_recodes")))
})

# UI output displaying information on l-diversity risk-measure
output$ui_rescat_ldiv_header <- renderUI({
  txt <- paste0("Here you can compute the ",tags$i("l"),"-diversity of sensitive variables. A dataset ")
  txt <- paste0(txt, "satisfies ",tags$i("l"),"-diversity if for every key ",tags$i("k")," there are at least ")
  txt <- paste0(txt, tags$i("l"), "different values for each of the sensitive variables. The statistics refer to the value of ",tags$i("l")," for each record.")
  out <- fluidRow(
    column(12, h3("l-diversity risk measure"), offset = 0, class = "wb-header"),
    column(12, p(HTML(txt)), offset = 0, class = "wb-header-hint"))
  out
})

output$ui_rescat_ldiv <- renderUI({
  # sensitive variable
  output$ldiv_sensvar <- renderUI({
    vv <- setdiff(allVars(), c(get_weightVar_name(), get_keyVars_names()))
    selectInput("ldiv_sensvar", label=p("Select one or more sensitive variables"), selected=input$ldiv_sensvar, choices=vv, multiple=TRUE, width="100%")
  })
  # recursive constant
  output$ldiv_recconst <- renderUI({
    txt_tooltip <- ""
    sliderInput("ldiv_recconst",
      label=p("Select a value for the recursive constant", tipify(icon("circle-info"), title=txt_tooltip, placement="top")),
      min=1, max=100, value=2, width="100%")
  })
  # button
  output$ldiv_btn <- renderUI({
    req(input$ldiv_sensvar, input$ldiv_recconst)
    if (length(input$ldiv_sensvar)==0) {
      return(NULL)
    }
    myActionButton("btn_ldiv", label="Calculate l-diversity risk measure", btn.style="primary")
  })

  output$ldiv_resetbtn <- renderUI({
    myActionButton("btn_ldiv_restart", label="Reset to choose different input parameters", btn.style="danger")
  })

  # ldiversity-results
  output$ldiv_summary <- renderUI({
    output$ldiv_summary_table <- renderTable({
      df_summary <- get_ldiv_result()$df
      if (is.null(df_summary)) {
        return(NULL)
      }
      df_summary
    }, row.names=FALSE)
    req(input$ldiv_sensvar)
    out <- fluidRow(
      column(12, h5("Summary statistics on distinct ldiversity"), align="center"),
      column(12, tableOutput("ldiv_summary_table"), align="center"))
    out
  })

  #  data table showing violating obs
  output$ldiv_violating <- renderUI({
    output$ldiv_violating_tab <- DT::renderDataTable({
      get_ldiv_result()$tab
    }, options=list(scrollX=TRUE), rownames=FALSE)
    req(input$ldiv_sensvar)
    res <- get_ldiv_result()
    if (is.null(res)) {
      return(NULL)
    }
    tab <- res$tab
    if (nrow(tab)>0) {
      out <- fluidRow(
        column(12, h5("The following",nrow(tab),"records violate l-diversity in at least one sensible variable:"), align="center"),
        column(12, DT::dataTableOutput("ldiv_violating_tab"), align="center")
      )
    } else {
      out <- fluidRow(
        column(12, p("No records violate l-diversity with the selected settings!"), align="center")
      )
    }
    out
  })

  res <- get_ldiv_result()

  if (is.null(res)) {
    out <- fluidRow(
      column(6, uiOutput("ldiv_sensvar"), align="center"),
      column(6, uiOutput("ldiv_recconst"), align="center")
    )
    out <- list(out, fluidRow(column(12, uiOutput("ldiv_btn"), align="center")))
  } else {
    out <- fluidRow(column(12, uiOutput("ldiv_resetbtn"), align="center"))
  }

  if (!is.null(res)) {
    out <- list(out, fluidRow(
      column(12, uiOutput("ldiv_summary"), align="center"),
      column(12, uiOutput("ldiv_violating"))
    ))
  }
  return(out)
})

# UI output for suda2-risk measure
output$ui_rescat_suda2_header <- renderUI({
  out <- fluidRow(
    column(12, h3("SUDA2 risk measure"), offset = 0, class = "wb-header"),
    column(12, p("The SUDA algorithm is used to search for Minimum Sample Uniques (MSU) in the data among the sample uniques to determine
               which sample uniques are also special uniques i.e., have subsets that are also unique. See the help files for more
               information on SUDA scores."), offset = 0, class = "wb-header-hint"))
  out
})
output$ui_rescat_suda2 <- renderUI({
  # DisFraction
  output$suda2_disf <- renderUI({
    txt_tooltip <- "This is the sampling fraction for a simple random sample and the common sampling fraction for stratified samples. The defaut value is 0.1, which corresponds to a 10 percent sample. "
    txt_tooltip <- paste0(txt_tooltip, "Note that SUDA is sensitive to the sampling fraction and a wrong value can produce distorted results.")
    sliderInput("suda2_disf",
      label=p("Specify the sampling fraction for the stratified sampling", tipify(icon("circle-info"), title=txt_tooltip, placement="top")),
      min=0.01, max=0.5, step=0.01,value=0.1, width="100%")
  })
  # button
  output$suda2_btn <- renderUI({
    req(input$suda2_disf)
    myActionButton("btn_suda2", label="Calculate suda2-scores", btn.style="primary")
  })
  output$suda2_resetbtn <- renderUI({
    myActionButton("btn_suda2_restart", label="Reset to choose a different sampling fraction parameter", btn.style="danger")
  })
  # suda2-results
  output$suda2_result <- renderUI({
    suda2 <- get_suda2_result()
    if (is.null(suda2)) {
      return(NULL)
    }

    fluidRow(
      column(12, h5(strong(paste0("Suda scores (sampling fraction is ",suda2$DisFraction,")"))), align="center"),
      column(12, p("The table below shows the frequencies of the records with a suda score in the specified intervals."), align="center"),
      column(12, renderTable(suda2$thresholds), align="center"),
      column(12, h5(strong("Attribute contributions")), align="center"),
      column(12, p("The table below shows the contribution of each categorical key variable to the SUDA scores. The contribution of a variable is the percentage of the total MSUs in the file that include this variable."), align="center"),
      column(12, renderTable(suda2$attribute_contributions), align="center")
    )
  })

  # suda2 can only be calculated for sdcProblems with >= 3 categorical key variables
  if (length(get_keyVars())<=2) {
    return(fluidRow(
      column(12, p("Suda2 scores can only be computed for scenarios with",code(">= 3"),"categorical key variables!", align="center"), class = "wb-warning-toast")
    ))
  }

  if (is.null(get_suda2_result())) {
    out <- fluidRow(
      column(12, uiOutput("suda2_disf"), align="center"),
      column(12, uiOutput("suda2_btn"), align="center")
    )
  } else {
    out <- fluidRow(
      column(12, uiOutput("suda2_resetbtn"), align="center"),
      column(12, uiOutput("suda2_result"), align="center")
    )
  }
  return(out)
})

# information on k-anonymity
output$ui_rescat_violating_kanon_header <- renderUI({
  out <- fluidRow(
    column(12, h3("Observations violating k-anonymity"), offset = 0, class = "wb-header"),
    column(12, p("Here you can browse the records that violate k-anonymity for a specified level of k."), offset = 0, class = "wb-header-hint"))
  out
})
output$ui_rescat_violating_kanon <- renderUI({
  output$violating_obs_tab <- DT::renderDataTable({
    risks <- get_risk()
    ii <- which(risks$fk<input$k_val_violating)
    if (length(ii)==0) {
      return(NULL)
    }
    df <- cbind(get_manipKeyVars()[ii,], risks[ii,])
    df$risk <- formatC(df$risk, format="f", digits=3)
    df[order(df$fk),]
  }, options = list(scrollX=TRUE))

  output$ui_kanon_selection <- renderUI({
    txt_tooltip <- "All records violating the k-anonymity for k equal to the set threshold are displayed."
    sl <- sliderInput("k_val_violating",
      label=p("Select value for 'k'", tipify(icon("circle-info"), title=txt_tooltip, placement="top")),
      value=3, min=1, max=50, step=1, width="100%")
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
    uiOutput("ui_kanon_selection"),
    uiOutput("ui_kanon_result"),
    fluidRow(column(12, DT::dataTableOutput("violating_obs_tab"))))
  out
})

output$ui_catvar1 <- renderUI({
  kv <- c(get_keyVars_names(), get_pramVars_names())
  if (length(kv)>1) {
    kv <- setdiff(kv, input$sel_catvar2)
  }
  if (is.null(input$sel_catvar1)) {
    sel <- kv[1]
  } else {
    sel <- input$sel_catvar1
  }
  selectInput("sel_catvar1", label=p("Variable 1"), choices=kv, selected=sel, width="100%")
})
output$ui_catvar2 <- renderUI({
  kv <- c("none",setdiff(c(get_keyVars_names(), get_pramVars_names()), input$sel_catvar1))
  if (length(kv)==0) {
    return(NULL)
  }
  if (is.null(input$sel_catvar2)) {
    sel <- "none"
  } else {
    if (input$sel_catvar2==input$sel_catvar1) {
      sel <- "none"
    } else {
      sel <- input$sel_catvar2
    }
  }
  selectInput("sel_catvar2", label=p("Variable 2"), choices=kv, selected=sel, width="100%")
})

# UI output for mosaicplot of one or two categorical key-variables
output$ui_rescat_mosaicplot_header <- renderUI({
  out <- fluidRow(
    column(12, h3("Graphical representation of original and modified data"), offset = 0, class = "wb-header"),
    column(12, p("Here you can view univariate bar plots and bivariate mosaic plots of categorical variables to compare the variables before and after applying anonymization methods."), offset = 0, class = "wb-header-hint")
  )
  out
})
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
    pV <- get_manipPramVars()
    if (!is.null(pV)) {
      df <- cbind(get_manipKeyVars(), pV)
    } else {
      df <- get_manipKeyVars()
    }
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
    uiOutput("ui_mosaic_selection"),
    fluidRow(column(12, h4("Original data"), align="center")),
    fluidRow(column(12, plotOutput("mosaicplot_o", height="600px"))),
    fluidRow(column(12, h4("Modified data"), align="center")),
    fluidRow(column(12, plotOutput("mosaicplot_m", height="600px"))))
  out
})

# UI output for bivariate tabulation of (modified) key variables
output$ui_bivariate_tab_header <- renderUI({
  out <- fluidRow(
    column(12, h3("Tabular representation of original and modified data"), offset = 0, class = "wb-header"),
    column(12, p("Here you can view univariate and bivariate tabulations of categorical key variables to compare the variables before and after applying anonymization methods."), offset = 0, class = "wb-header-hint")
  )
  out
})
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
      tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
      tab$Freq <- as.integer(tab$Freq)
      colnames(tab) <- c(vars[1], "Freq")
    } else {
      tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
      colnames(tab)[is.na(colnames(tab))] <- "NA"
      rownames(tab)[is.na(rownames(tab))] <- "NA"
      tab <- cbind(rownames(tab), tab)
      colnames(tab)[1] <- ""
    }
    tab
  }, include.rownames = FALSE)
  output$biv_tab_m <- renderTable({
    req(input$sel_catvar1)
    if (!is.null(get_manipPramVars())) {
      df <- cbind(get_manipKeyVars(), get_manipPramVars())
    } else {
      df <- get_manipKeyVars()
    }
    vars <- c(input$sel_catvar1, input$sel_catvar2)
    if (vars[2]=="none") {
      tab <- as.data.frame.table(addmargins(table(df[[vars[1]]], useNA="always")))
      tab$Freq <- as.integer(tab$Freq)
      colnames(tab) <- c(vars[1], "Freq")
    } else {
      tab <- as.data.frame.matrix(formatC(addmargins(table(df[[vars[1]]], df[[vars[2]]], useNA = "always")), format = "f", digits = 0))
      colnames(tab)[is.na(colnames(tab))] <- "NA"
      rownames(tab)[is.na(rownames(tab))] <- "NA"
      tab <- cbind(rownames(tab), tab)
      colnames(tab)[1] <- ""
    }
    tab
  }, include.rownames = FALSE)
  output$biv_tab_res <- renderPrint({
    req(input$sel_catvar2)
    if(!is.null(input$sel_catvar2) & input$sel_catvar2 == "none"){
      tabs <- fluidRow(column(6, h4("Original data"), align="center"),
                       column(6, h4("Modified data"), align="center"),
                       column(6, tableOutput("biv_tab_o"), align="center"),
                       column(6, tableOutput("biv_tab_m"), align="center"))
    } else {
      tabs <- fluidRow(column(12, h4("Original data"), align="center"),
                       column(12, tableOutput("biv_tab_o"), align="center", class="wn-info-table wn-row-title"),
                       column(12, h4("Modified data"), align="center"),
                       column(12, tableOutput("biv_tab_m"), align="center", class="wn-info-table wn-row-title"))
    }
    tabs
  })

  out <- list(uiOutput("ui_biv_selection"), uiOutput("biv_tab_res"))

  out
})
