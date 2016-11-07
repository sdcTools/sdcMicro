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
    sel_var <- selectInput("sel_topbot_var_num", choices=numVars(), selected=obj$inp_sel_topbot_var_num, multiple=FALSE, label=h5("Select variable"), width="75%")
    sel_kind <- radioButtons("sel_topbot_kind_num", choices=c("top","bottom"), label=h5("Apply Top/Bottom-Coding?"), inline=TRUE)
    txt_val <- textInput("num_topbot_val_num", label=h5("Threshold value"), placeholder="Please enter a number", width="75%")
    txt_replace <- textInput("num_topbot_replacement_num", label=h5("Replacement Value"), placeholder="Please enter a number", width="75%")
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

  out <- fluidRow(
    column(12, h4("Apply Top/Bottom-Coding", align="center")),
    column(12, p("This page allows to replace values above (Top-Coding) or below (Bottom-Coding) a threshold with a custom number."), align="center")
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
    rb_clustermethod <- radioButtons("rb_microagg_clustermethod", label=h5("Clustermethod"),
      choices=c("clara","pam","kmeans","cmeans","bclust"),
      width="100%", selected=input$rb_microagg_clustermethod, inline=TRUE)
    rb_transf <- radioButtons("rb_microagg_transf", label=h5("Transformation"),
      choices=c("none","log","boxcox"), width="100%", selected=input$rb_microagg_transf, inline=TRUE)
    sl_nc <- isolate(sliderInput("sl_microagg_nc", label=h5("Number of clusters"), min=1, max=15, step=1, value=3, width="100%"))

    return(fluidRow(
      column(4, rb_clustermethod, align="center"),
      column(4, rb_transf, align="center"),
      column(4, sl_nc, align="center")
    ))
  })

  # stratification variable for PRAM
  output$ui_microagg_strata <- renderUI({
    selectInput("sel_microagg_strata", label=h5("Apply microaggregation in groups (stratification)?"),
        choices=c("no stratification", poss_strataVarP()), multiple=FALSE, width="100%", selected=input$sel_microagg_strata)
  })

  rb_clbased <- radioButtons("rb_microagg_cluster", label=h5("Use a cluster-based method?"),
    choices=c("No","Yes"), selected=input$rb_microagg_cluster, inline=TRUE, width="100%")
  sel_method <- selectInput("sel_microagg_method", label=h5("Select the method"),
    choices=choices_aggmethods(), selected=input$sel_microagg_method, width="100%")
  sel_microvars <- selectInput("sel_microagg_v", choices=possvars_numericmethods(),
    label=h5("Select Variables for Microaggregation"), selected=input$sel_microagg_v, width="100%", multiple=TRUE)

  out <- fluidRow(
    column(12, h4("Microaggregation for numerical variables", align="center")),
    column(12, p("Many different algorithms to microaggregate (some) or all numeric key variables can be selected here. The most important
      parameter is the",code("aggregation level"), "because it specifies how many observations are grouped together before replacing actual values with some kind of aggregate.", align="center")))

  out <- list(out, fluidRow(
    column(4, rb_clbased, align="center"),
    column(4, sel_method, align="center"),
    column(4, uiOutput("ui_microagg_strata"), align="center")
  ))

  # simple, onedims, pca, mcdpca, pppca: --> aggr, measure, trim
  # single: --> aggr, measure, trim, varsort
  # influence, clustpca, clustmcdpca, clustpppca: --> aggr, measure, trim, clustermethod, transf, nc
  # rmd, mdav: aggr,
  if ( !is.null(input$sel_microagg_method) ) {
    sl1 <- sliderInput("sl_microagg_aggr", label=h5("Aggregation-Level"), min=1, max=15, step=1, value=3, width="100%")
    if (has_numkeyvars()) {
      out <- list(out, fluidRow(
        column(6, sl1, align="center"),
        column(6, tipify(sel_microvars, "If empty, the specified numerical key variables will be used!"), align="center")))
    } else {
      out <- list(out, fluidRow(
        column(6, sl1, align="center"),
        column(6, sel_microvars, align="center")))
    }

    if (!input$sel_microagg_method %in% c("mdav","rmd")) {
      rb_measure <- radioButtons("sl_microagg_measure", label=h5("Aggregation Statistics"), choices=c("mean", "median", "trim", "onestep"),
        width="100%", selected=input$sl_microagg_measure, inline=TRUE)
      sl_trim <- sliderInput("sl_microagg_trim", label=h5("Trimming-Percentage"), min=0, max=0.5, step=0.01, value=0, width="100%")

      if (input$sel_microagg_method=="single") {
        sel_varsort <- selectInput("sel_microagg_varsort", label=h5("Select variable for sorting"),
          choices=get_allNumericVars_name(), selected=input$sel_microagg_varsort, width="100%")
        out <- list(out, fluidRow(
          column(4, rb_measure, align="center"),
          column(4, sl_trim, align="center"),
          column(4, sel_varsort, align="center")))
      } else {
        out <- list(out, fluidRow(
          column(6, rb_measure, align="center"),
          column(6, sl_trim, align="center")))
      }
      # cluster-based methods
      if (input$rb_microagg_cluster=="Yes") {
        out <- list(out, uiOutput("ui_microagg_clusterbased"))
      }
    }
  }
  btn_microagg <- myActionButton("btn_microagg", label="Perform Microaggregation", "primary")
  if (has_numkeyvars() | length(input$sel_microagg_v)>0 ) {
    out <- list(out, fluidRow(column(12, p(btn_microagg, align="center"))))
  }
  out
})

# GUI-output for addNoise()
output$ui_noise <- renderUI({
  # returns possible methods for addNoise()
  # 'correlated' needs at least two columns=variables
  choices_noise <- reactive({
    m <- c("additive","correlated2","restr","ROMM","outdect")
    if (length(input$sel_noise_v) >=2 | length(get.sdcMicroObj(sdcObj(), type="numVars")) >= 2) {
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
    input$sel_noise_method
    isolate({
      if (is.null(input$sel_noise_method)) {
        return(NULL)
      }
      if (input$sel_noise_method=="correlated2") {
        lab <- h4("Parameter 'delta'")
        par <- c(value=0.1, min=0.1, max=2, step=0.01)
      } else if (input$sel_noise_method=="ROMM") {
        lab <- h4("Parameter 'p'")
        par <- c(value=0.001, min=0.001, max=0.3, step=0.001)
      } else {
        lab <- h4("Amount of noise")
        par <- c(value=150, min=0, max=300, step=1)
      }
      sliderInput("sl_noise_noise", label=lab,
        min=par["min"], max=par["max"], step=par["step"], value=par["value"], width="100%")
    })
  })

  # ui for selection of method
  output$ui_noise_method <- renderUI({
    input$sel_noise_method
    isolate({
      sel_method <- selectInput("sel_noise_method", label=h5("Select the algorithm"),
        choices=choices_noise(),
        selected=input$sel_noise_method, width="100%")
      sel_method
    })
  })

  # variables selected
  output$ui_noise_vars <- renderUI({
    input$sel_noise_method
    isolate({
      lab <- h5("Select variables")
      if (has_numkeyvars()) {
        lab <- list(lab, p("If empty, all numerical key variables will be used!"))
      }
      sel_noisevars <- selectInput("sel_noise_v", choices=possvars_numericmethods(),
        label=lab, selected=input$sel_noise_v, width="100%", multiple=TRUE)
      sel_noisevars
    })
  })

  # ui for 'btn_noise
  output$ui_noise_btn <- renderUI({
    btn <- NULL
    if (has_numkeyvars() | length(input$sel_noise_v)>0) {
      btn <- myActionButton("btn_noise", label="Apply addNoise()", "primary")
    }
    btn
  })

  out <- fluidRow(
    column(12, h4("Adding Stochastic Noise", align="center")),
    column(12, p("Various methods for adding noise to perturb continuous scaled variables can be selected below. Please note, that even if a
      stratification variable has been defined during the initialization of the sdc-Problem, this information will not be used in this case. So the noise will be applied on
      the entire data set.", align="center")))

  out <- list(out, fluidRow(
    column(6, uiOutput("ui_noise_method")),
    column(6, uiOutput("ui_noise_slider"))))

  out <- list(out, fluidRow(
    column(3, ""),
    column(6, uiOutput("ui_noise_vars")),
    column(3, "")))

  out <- list(out, fluidRow(column(12, uiOutput("ui_noise_btn"))))
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
      btn <- myActionButton("btn_rankswap", label="Apply rankSwap()", "primary")
    }
    btn
  })

  output$ui_rankswap_vars <- renderUI({
    lab <- h5("Select Variables")
    if (has_numkeyvars()) {
      lab <- list(lab, p("If empty, all numerical key variables will be used!"))
    }
    sel_rankswapvars <- selectInput("sel_rankswap_v", choices=possvars_numericmethods(),
      label=lab, selected=input$sel_rankswap_v, width="100%", multiple=TRUE)
    sel_rankswapvars
  })

  sl_rankswap_top <- isolate(sliderInput("sl_rankswap_top", label=h4("Percentage of largest values that are grouped together before rank swapping"), min=0, max=25, step=1, value=0, width="100%"))
  sl_rankswap_bot <- isolate(sliderInput("sl_rankswap_bot", label=h4("Percentage of lowest values that are grouped together before rank swapping"), min=0, max=25, step=1, value=0, width="100%"))
  sl_rankswap_k0 <- isolate(sliderInput("sl_rankswap_k0", label=h4("Subset-mean preservation factor"), min=0, max=1, step=0.01, value=0, width="100%"))
  sl_rankswap_r0 <- isolate(sliderInput("sl_rankswap_r0", label=h4("Multivariate preservation factor"), min=0, max=1, step=0.01, value=0.95, width="100%"))
  sl_rankswap_p <- isolate(sliderInput("sl_rankswap_p", label=h4("Rank range as percentage of total sample size."), min=0, max=100, step=1, value=0, width="100%"))


  out <- fluidRow(
    column(12, h4("Rank Swapping", align="center")),
    column(12, p("This is a method to be used on numeric or ordinal variables. The idea is to",tags$i("swap"),"values within a range
      so that correlation structure of original variables are preserved and also some perturbation is applied. Please note, that even if a
      stratification variable has been defined during the initialization of the sdc-Problem, this information will not be used in this case. So the noise will be applied on
      the entire data set.", align="center")))

  out <- list(out, fluidRow(
    column(4, uiOutput("ui_rankswap_vars")),
    column(4, p(sl_rankswap_bot, align="center")),
    column(4, p(sl_rankswap_top, align="center"))))

  out <- list(out, fluidRow(
    column(4, sl_rankswap_k0),
    column(4, p(sl_rankswap_r0, align="center")),
    column(4, p(sl_rankswap_p, align="center"))))

  out <- list(out, fluidRow(
    column(12, uiOutput("ui_rankswap_btn"))))
  out
})
