# returns TRUE if numeric key-variables exist in obj$sdcObj
has_numkeyvars <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  length(get.sdcMicroObj(curObj, type="numVars"))>0
})

# UI-output for top/bottom-coding of numerical variables
output$ui_topbotcoding_num <- renderUI({
  output$ui_topbot_plot_num <- renderPlot({
    req(input$sel_topbot_var_num)

    curV <- obj$inp_sel_topbot_var_num
    if (is.null(curV)) {
      return(NULL)
    }
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(NULL)
    }

    if (curV %in% get_numVars_names()) {
      vv <- curObj@manipNumVars[[curV]]
    } else {
      vv <- curObj@origData[[curV]]
    }
    boxplot(vv, main=input$sel_topbot_var_num, xlab=input$sel_topbot_var_num, col="#DADFE1")
  })
  output$ui_topbot_params_num <- renderUI({
    txt_tooltip1 <- "In case of top, all values above the threshold are replaced, in the case of bottom, all values below the threshold are replaced."
    txt_tooltip2 <- "All values below (bottom) or above (top) this threshold are replaced with the replacement value."
    txt_tooltip3 <- "The replacement value is the value that replaces all the values below (bottom) or above (top) the specified threshold. Often the replacement value is the same as the threshold value."
    sel_var <- selectInput("sel_topbot_var_num", choices=numVars(), selected=obj$inp_sel_topbot_var_num, multiple=FALSE, label=h5("Select variable"), width="75%")
    sel_kind <- radioButtons("sel_topbot_kind_num", choices=c("top","bottom"),
      label=h5("Apply Top/Bottom-Coding?", tipify(icon("question"), title=txt_tooltip1, placement="top")), inline=TRUE)
    txt_val <- textInput("num_topbot_val_num", label=h5("Threshold value", tipify(icon("question"), title=txt_tooltip2, placement="top")),
      placeholder="Please enter a number", width="75%")
    txt_replace <- textInput("num_topbot_replacement_num", label=h5("Replacement Value", tipify(icon("question"), title=txt_tooltip3, placement="top")),
      placeholder="Please enter a number", width="75%")
    out <- fluidRow(column(6, sel_var, align="center"), column(6, sel_kind, align="center"))
    out <- list(out, fluidRow(column(6, txt_val, align="center"), column(6, txt_replace, align="center")))
    out
  })
  observeEvent(input$sel_topbot_var_num,{
    obj$inp_sel_topbot_var_num <- input$sel_topbot_var_num
  })
  output$ui_topbot_btn_num <- renderUI({
    req(input$sel_topbot_var_num)
    curObj <- sdcObj()
    if (is.null(curObj)) {
      return(NULL)
    }
    num1 <- suppressWarnings(as.numeric(input$num_topbot_val_num))
    num2 <- suppressWarnings(as.numeric(input$num_topbot_replacement_num))
    if (is.na(num1) || is.na(num2)) {
      return(NULL)
    }
    if (is.null(num1) || is.null(num2)) {
      return(NULL)
    }
    if (is.numeric(num1) & is.numeric(num2)) {
      if (input$sel_topbot_var_num %in% get_numVars_names()) {
        vv <- curObj@manipNumVars[[input$sel_topbot_var_num]]
      } else {
        vv <- curObj@origData[[input$sel_topbot_var_num]]
      }
      if (input$sel_topbot_kind_num=="top") {
        n <- sum(vv >= num1, na.rm=TRUE)
      } else {
        n <- sum(vv <= num1, na.rm=TRUE)
      }
      N <- length(na.omit(vv))
      p <- formatC(100*(n/N), format="f", digits=2)
      return(fluidRow(
        column(12, p(code(n),"(out of",code(N),") values will be replaced! This equals",code(p),"percent of the data."), align="center"),
        column(12, myActionButton("btn_topbotcoding_num",label=("Apply Top/Bottom-Coding"), "primary"), align="center")
      ))
    } else {
      return(NULL)
    }
  })

  helptxt <- "Here you can recode all values in a variable below (bottom-coding) or above (top-coding) a certain threshold. These values are replaced"
  helptxt <- paste(helptxt, "with the specified replacement value.")
  out <- fluidRow(
    column(12, h4("Apply Top- and bottom coding", align="center")),
    column(12, p(helptxt), align="center")
  )
  out <- list(out, uiOutput("ui_topbot_params_num"))
  out <- list(out, uiOutput("ui_topbot_btn_num"))
  out <- list(out, fluidRow(
    column(12, plotOutput("ui_topbot_plot_num"))
  ))
  out
})

# GUI-output for microaggregation()
output$ui_microaggregation <- renderUI({
  # returns possible methods for microaggregation
  choices_aggmethods <- reactive({
    if (is.null(input$rb_microagg_cluster)) {
      return(NULL)
    }
    if (input$rb_microagg_cluster=="Yes") {
      return(c("influence", "clustpca", "clustmcdpca", "clustpppca"))
    } else {
      return(c("mdav", "rmd", "simple", "single","onedims", "pca", "mcdpca", "pppca"))
    }
  })

  # additional ui-elements for cluster-based methods
  output$ui_microagg_clusterbased <- renderUI({
    txt_clmethod <- ""
    txt_cltransf <- ""
    txt_clnrcl <- ""
    rb_clustermethod <- radioButtons("rb_microagg_clustermethod",
      label=h5("Clustermethod", tipify(icon("question"), title=txt_clmethod, placement="top")),
      choices=c("clara","pam","kmeans","cmeans","bclust"), width="100%", selected=input$rb_microagg_clustermethod, inline=TRUE)
    rb_transf <- radioButtons("rb_microagg_transf",
      label=h5("Transformation", tipify(icon("question"), title=txt_cltransf, placement="top")),
      choices=c("none","log","boxcox"), width="100%", selected=input$rb_microagg_transf, inline=TRUE)
    sl_nc <- isolate(sliderInput("sl_microagg_nc",
      label=h5("Number of clusters", tipify(icon("question"), title=txt_clnrcl, placement="top")),
      min=1, max=15, step=1, value=3, width="100%"))

    return(fluidRow(
      column(4, rb_clustermethod, align="center"),
      column(4, rb_transf, align="center"),
      column(4, sl_nc, align="center")
    ))
  })
  output$ui_microagg_strata <- renderUI({
    txt_tooltip <- "By default maicroaggregation is applied on the complete dataset. To apply the algorithm within strata, select a variable for stratification. The algorithm is then applied within the strata defined by the factor levels of that variable."
    selectInput("sel_microagg_strata",
      label=h5("Apply microaggregation in groups (stratification)?", tipify(icon("question"), title=txt_tooltip, placement="top")),
      choices=c("no stratification", poss_strataVarP()), multiple=FALSE, width="100%", selected=input$sel_microagg_strata)
  })
  output$ui_microagg_use_cluster <- renderUI({
    txt_tooltip <- ""
    radioButtons("rb_microagg_cluster",
      label=h5("Use a cluster-based method?", tipify(icon("question"), title=txt_tooltip, placement="top")),
      choices=c("No","Yes"), selected=input$rb_microagg_cluster, inline=TRUE, width="100%")
  })
  output$ui_microagg_method <- renderUI({
    txt <- "MDAV is the default method. The different methods differ with respect to the distance measure used to measure the proximity between two values.<br />"
    txt <- paste(txt, "<strong>mdav</strong> - grouping is based on classical (Euclidean) distance measures<br />")
    txt <- paste(txt, "<strong>rmd</strong> - grouping is based on robust multivariate (Mahalanobis) distance measures<br />")
    txt <- paste(txt, "<strong>pca</strong> - grouping is based on principal component analysis whereas the data are sorted on the first principal component<br />")
    txt <- paste(txt, "<strong>clustpppca</strong> - add here<br />")
    txt <- paste(txt, "<strong>mdcpca</strong> - add here<br />")
    txt <- paste(txt, "<strong>single</strong> - add here<br />")
    txt <- paste(txt, "<strong>simple</strong> - add here<br />")
    txt <- paste(txt, "<strong>onedims</strong> - add here<br />")
    txt <- paste(txt, "<strong>pppca</strong> - add here<br /><br />")
    txt <- paste(txt, "<strong>Cluster-based methods:</strong><br />")
    txt <- paste(txt, "<strong>influence</strong> - grouping is based on clustering and aggregation is performed within clusters<br /><br />")
    txt <- paste(txt, "<strong>clustpca</strong> - add here<br /><br />")
    txt <- paste(txt, "<strong>clustmcdpca</strong> - add here<br /><br />")
    txt <- paste(txt, "<strong>clustpppca</strong> - grouping is based on clustering and (robust) principal component analysis for each cluster<br /><br />")
    sel_method <- selectInput("sel_microagg_method",
      label=h5("Select the method", tipify(icon("question"), title=txt, placement="bottom")),
      choices=choices_aggmethods(), selected=input$sel_microagg_method, width="100%")
  })
  output$ui_microagg_vars <- renderUI({
    selectInput("sel_microagg_v", choices=possvars_numericmethods(),
      label=h5("Select Variables for Microaggregation"),
      selected=input$sel_microagg_v, width="100%", multiple=TRUE)
  })
  output$ui_microagg_agglevel <- renderUI({
    txt_tooltip <- "Specifies the group size"
    sliderInput("sl_microagg_aggr",
      label=h5("Aggregation-Level", tipify(icon("question"), title=txt_tooltip, placement="top")),
      min=1, max=15, step=1, value=3, width="100%")
  })
  output$ui_microagg_aggmeasures <- renderUI({
    txt_tooltip <- "Specifies the group statistic that replaces all values in a group."
    radioButtons("sl_microagg_measure",
      label=h5("Aggregation Statistics", tipify(icon("question"), title=txt_tooltip, placement="top")),
      choices=c("mean", "median", "trim", "onestep"), width="100%", selected=input$sl_microagg_measure, inline=TRUE)
  })
  output$ui_microagg_trim <- renderUI({
    txt_tooltip <- "Specifies the group statistic that replaces all values in a group."
    sliderInput("sl_microagg_trim",
      label=h5("Trimming-Percentage", tipify(icon("question"), title=txt_tooltip, placement="top")),
      min=0, max=0.5, step=0.01, value=0, width="100%")
  })
  output$ui_microagg_varsort <- renderUI({
    txt_tooltip <- ""
    sel_varsort <- selectInput("sel_microagg_varsort",
      label=h5("Select variable for sorting", tipify(icon("question"), title=txt_tooltip, placement="top")),
      choices=get_allNumericVars_name(), selected=input$sel_microagg_varsort, width="100%")
  })

  out <- fluidRow(
    column(12, h4("Microaggregation for numerical variables", align="center")),
    column(12, p("Many different algorithms to microaggregate numeric key variables can be applied here. The most important
      parameter is the",code("aggregation level"), "because it specifies how many observations are grouped together before replacing actual values with some kind of aggregate.", align="center")))

  out <- list(out, fluidRow(
    column(4, uiOutput("ui_microagg_use_cluster"), align="center"),
    column(4, uiOutput("ui_microagg_method"), align="center"),
    column(4, uiOutput("ui_microagg_strata"), align="center")
  ))

  # simple, onedims, pca, mcdpca, pppca: --> aggr, measure, trim
  # single: --> aggr, measure, trim, varsort
  # influence, clustpca, clustmcdpca, clustpppca: --> aggr, measure, trim, clustermethod, transf, nc
  # rmd, mdav: aggr,
  if ( !is.null(input$sel_microagg_method) ) {
    out <- list(out, fluidRow(
      column(6, uiOutput("ui_microagg_agglevel"), align="center"),
      column(6, uiOutput("ui_microagg_vars"), align="center")))
    if (!input$sel_microagg_method %in% c("mdav","rmd")) {
      if (input$sel_microagg_method=="single") {
        out <- list(out, fluidRow(
          column(4, uiOutput("ui_microagg_aggmeasures"), align="center"),
          column(4, uiOutput("ui_microagg_trim"), align="center"),
          column(4, uiOutput("ui_microagg_varsort"), align="center")))
      } else {
        out <- list(out, fluidRow(
          column(6, uiOutput("ui_microagg_aggmeasures"), align="center"),
          column(6, uiOutput("ui_microagg_trim"), align="center")))
      }
      # cluster-based methods
      if (input$rb_microagg_cluster=="Yes") {
        out <- list(out, uiOutput("ui_microagg_clusterbased"))
      }
    }
  }
  btn_microagg <- myActionButton("btn_microagg", label="Perform Microaggregation", "primary")
  if (has_numkeyvars() | length(input$sel_microagg_v)>0 ) {
    out <- list(out, fluidRow(column(12, btn_microagg, align="center")))
  }
  out
})

# GUI-output for addNoise()
output$ui_noise <- renderUI({
  # returns possible methods for addNoise()
  # 'correlated' needs at least two columns=variables
  choices_noise <- reactive({
    m <- c("additive","correlated2","restr","ROMM","outdect")
    if (length(input$sel_noise_v) >=2 | length(get_numVars_names()) >= 2) {
      m <- c(m, "correlated")
    }
    m
  })
  # this needs to be a bit complicated because otherwise sliders would be constantly
  # resetting to default values!
  # also we have different parameters for methods (noise, p, delta) that we
  # catch with a single slider
  if (!has_numkeyvars()) {
    return(fluidRow(
      column(12, h4("The current sdcProblem contains no",code("numerical key variables")), align="center"),
      column(12, p("However,",code("addNoise()"),"can only be applied on such variables!
        Please modify the",code("sdcProblem"), align="center"))))
  }

  # ui for slider defining noise, p or delta
  output$ui_noise_slider <- renderUI({
    req(input$sel_noise_method)
    if (input$sel_noise_method=="correlated2") {
      txt_tooltip <- "The added noise is proportional to the variance in the data. The amount of noise is the multiplier for the standard deviation of the noise."
      lab <- h5("Amount of noise (parameter 'delta')", tipify(icon("question"), title=txt_tooltip, placement="top"))
      par <- c(value=0.1, min=0.1, max=2, step=0.01)
    } else if (input$sel_noise_method=="ROMM") {
      txt_tooltip <- ""
      lab <- h5("Amount of noise (parameter 'p')", tipify(icon("question"), title=txt_tooltip, placement="top"))
      par <- c(value=0.001, min=0.001, max=0.3, step=0.001)
    } else {
      txt_tooltip <- ""
      lab <- h5("Amount of noise (parameter 'noise')", tipify(icon("question"), title=txt_tooltip, placement="top"))
      par <- c(value=150, min=0, max=300, step=1)
    }
    sliderInput("sl_noise_noise", label=lab, min=par["min"], max=par["max"], step=par["step"], value=par["value"], width="100%")
  })

  # ui for selection of method
  output$ui_noise_method <- renderUI({
    txt_tooltip <- "<strong>Noise addition methods:</strong><br />"
    txt_tooltip <- paste0(txt_tooltip, "<strong>additive</strong> - adds noise completely at random to each variable depending on their size and standard deviation<br />")
    txt_tooltip <- paste0(txt_tooltip, "<strong>correlated2</strong> - adds noise and preserves the covariances<br />")
    txt_tooltip <- paste0(txt_tooltip, "<strong>restr</strong> - takes the sample size into account when adding noise<br />")
    txt_tooltip <- paste0(txt_tooltip, "<strong>ROMM</strong> - implementation of the algorithm ROMM (Random Orthogonalized Matrix Masking)<br />")
    txt_tooltip <- paste0(txt_tooltip, "<strong>outdect</strong> - adds noise only to outliers. The outliers are identified with univariate and robust multivariate procedures based on a robust mahalanobis distances calculated by the MCD estimator.<br />")
    txt_tooltip <- paste0(txt_tooltip, "<strong>correlated</strong> - adds noise and preserves the covariances")
    selectInput("sel_noise_method",
      label=h5("Select the algorithm", tipify(icon("question"), title=txt_tooltip, placement="bottom")),
      choices=choices_noise(), selected=input$sel_noise_method, width="100%")
  })

  # variables selected
  output$ui_noise_vars <- renderUI({
    selectInput("sel_noise_v", choices=get_numVars_names(), label=h5("Select variables"), width="75%", multiple=TRUE)
  })

  # ui for 'btn_noise
  output$ui_noise_btn <- renderUI({
    btn <- NULL
    if (has_numkeyvars() | length(input$sel_noise_v)>0) {
      btn <- myActionButton("btn_noise", label="Add noise", "primary")
    }
    btn
  })

  out <- fluidRow(
    column(12, h4("Adding Stochastic Noise", align="center")),
    column(12, p("Here you can use various methods to add noise in order to perturb continuous variables.", align="center")))

  out <- list(out, fluidRow(
    column(6, uiOutput("ui_noise_vars"), align="center"),
    column(6, uiOutput("ui_noise_method"), align="center")))

  out <- list(out, fluidRow(column(12, uiOutput("ui_noise_slider"), align="center")))
  out <- list(out, fluidRow(column(12, uiOutput("ui_noise_btn"), align="center")))
  out
})

# GUI-output for shuffling()
output$ui_shuffling <- renderUI({
  fluidRow(column(12, h4("Shuffling", align="center")))
})

# GUI-output for rankSwap()
output$ui_rankswap <- renderUI({
  if (!has_numkeyvars()) {
    return(fluidRow(
      column(12, h4("The current sdcProblem contains no",code("numerical key variables")), align="center"),
      column(12, p("However,",code("rankSwap()"),"can only be applied on such variables!
        Please modify the",code("sdcProblem"), align="center"))))
  }

  # ui for 'btn_rankswap
  output$ui_rankswap_btn <- renderUI({
    btn <- NULL
    if (has_numkeyvars() | length(input$sel_rankswap_v)>0) {
      btn <- myActionButton("btn_rankswap", label="Apply rank-swapping", "primary")
    }
    btn
  })

  output$ui_rankswap_vars <- renderUI({
    selectInput("sel_rankswap_v", choices=possvars_numericmethods(), label=h5("Select Variables"), width="100%", multiple=TRUE)
  })
  output$sl_rankswap_top <- renderUI({
    sliderInput("sl_rankswap_top", label=h5("Percentage of largest values that are grouped together before rank swapping"), min=0, max=25, step=1, value=0, width="100%")
  })
  output$sl_rankswap_bot <- renderUI({
    sliderInput("sl_rankswap_bot", label=h5("Percentage of lowest values that are grouped together before rank swapping"), min=0, max=25, step=1, value=0, width="100%")
  })
  output$sl_rankswap_k0 <- renderUI({
    txt_tooltip <- ""
    sliderInput("sl_rankswap_k0",
      label=h5("Subset-mean preservation factor", tipify(icon("question"), title=txt_tooltip, placement="top")),
      min=0, max=1, step=0.01, value=0, width="100%")
  })
  output$sl_rankswap_r0 <- renderUI({
    txt_tooltip <- ""
    sliderInput("sl_rankswap_r0",
      label=h5("Multivariate preservation factor", tipify(icon("question"), title=txt_tooltip, placement="top")),
      min=0, max=1, step=0.01, value=0.95, width="100%")
  })
  output$sl_rankswap_p <- renderUI({
    txt_tooltip <- ""
    sliderInput("sl_rankswap_p",
      label=h5("Rank range as percentage of total sample size.", tipify(icon("question"), title=txt_tooltip, placement="top")),
      min=0, max=100, step=1, value=0, width="100%")
  })

  out <- fluidRow(
    column(12, h4("Apply rank swapping"), align="center"),
    column(12, p("This is a method to be used on numeric or ordinal variables. The idea is to",tags$i("swap"),"values within a range
      so that correlation structure of original variables is preserved and some perturbation is applied."), align="center"))

  out <- list(out, fluidRow(
    column(4, uiOutput("ui_rankswap_vars"), align="center"),
    column(4, uiOutput("sl_rankswap_bot"), align="center"),
    column(4, uiOutput("sl_rankswap_top"), align="center")))

  out <- list(out, fluidRow(
    column(4, uiOutput("sl_rankswap_k0"), align="center"),
    column(4, uiOutput("sl_rankswap_r0"), align="center"),
    column(4, uiOutput("sl_rankswap_p"), align="center")))

  out <- list(out, fluidRow(
    column(12, uiOutput("ui_rankswap_btn"), align="center")))
  out
})
