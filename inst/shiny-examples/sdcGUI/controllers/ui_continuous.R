# returns TRUE if numeric key-variables exist in obj$sdcObj
has_numkeyvars <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  length(get.sdcMicroObj(obj$sdcObj, type="numVars"))>0
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
    rb_clustermethod <- radioButtons("rb_microagg_clustermethod", label=h4("Clustermethod"),
      choices=c("clara","pam","kmeans","cmeans","bclust"),
      width="100%", selected=input$rb_microagg_clustermethod, inline=TRUE)
    rb_transf <- radioButtons("rb_microagg_transf", label=h4("Transformation"),
      choices=c("none","log","boxcox"), width="100%", selected=input$rb_microagg_transf, inline=TRUE)
    sl_nc <- isolate(sliderInput("sl_microagg_nc", label=h4("Number of clusters"), min=1, max=15, step=1, value=3, width="100%"))

    return(fluidRow(
      column(4, rb_clustermethod),
      column(4, rb_transf),
      column(4, sl_nc)
    ))
  })

  output$ui_microagg_strata <- ui_custom_selectInput(
    choices=c("Use existing stratification-variable"="usedefault",
              "Do not use any stratification variable"="none",possStrataVars()),
    id="sel_microagg_strata", label="Stratification variable", multiple=FALSE)

  rb_clbased <- radioButtons("rb_microagg_cluster", label=h5("Do you want to use a cluster-based method?"),
    choices=c("No","Yes"), selected=input$rb_microagg_cluster, inline=TRUE, width="100%")
  sel_method <- selectInput("sel_microagg_method", label=h5("Select the method"),
    choices=choices_aggmethods(), selected=input$sel_microagg_method, width="100%")

  lab_microvars <- h5("Select Variables for Microaggregation")
  if (has_numkeyvars()) {
    lab_microvars <- list(lab_microvars, p("If empty, the specified numerical key variables will be used!"))
  }

  sel_microvars <- selectInput("sel_microagg_v", choices=possvars_numericmethods(),
    label=lab_microvars, selected=input$sel_microagg_v, width="100%", multiple=TRUE)

  out <- fluidRow(
    column(12, h4("Microaggregation for numerical variables", align="center")),
    column(12, p("Many different algorithms to microaggregate (some) or all numeric key variables can be selected here. The most important
      parameter is the",code("aggregation level"), "because it specifies how many observations are grouped together before replacing actual values with some kind of aggregate.", align="center")),
    column(12, p("By default, microaggregation is performed by strata (if a strata-variable has been selected in the creating of the sdc-Problem. However,
      This can be circumvented by selecting",tags$i("Do not use any stratification variable"),"in the corresponding selection menu. In this place, also other variables may be selected
      which will be used only for the microaggregation procedure as stratification variable.", align="center")),
    column(12, h4("Microaggregation for numerical variables", align="center")))

  out <- list(out, fluidRow(
    column(4, sel_method),
    column(4, rb_clbased),
    column(4, uiOutput("ui_microagg_strata"))
  ))

  # simple, onedims, pca, mcdpca, pppca: --> aggr, measure, trim
  # single: --> aggr, measure, trim, varsort
  # influence, clustpca, clustmcdpca, clustpppca: --> aggr, measure, trim, clustermethod, transf, nc
  # rmd, mdav: aggr,
  if ( !is.null(input$sel_microagg_method) ) {
    sl1 <- sliderInput("sl_microagg_aggr", label=h4("Aggregation-Level"), min=1, max=15, step=1, value=3, width="100%")

    out <- list(out, fluidRow(
      column(6, p(sl1, align="center")),
      column(6, sel_microvars)))

    if (!input$sel_microagg_method %in% c("mdav","rmd")) {
      rb_measure <- radioButtons("sl_microagg_measure", label=h4("Aggregation Statistics"), choices=c("mean", "median", "trim", "onestep"),
        width="100%", selected=input$sl_microagg_measure, inline=TRUE)
      sl_trim <- sliderInput("sl_microagg_trim", label=h4("Trimming-Percentage"), min=0, max=0.5, step=0.01, value=0, width="100%")

      if (input$sel_microagg_method=="single") {
        sel_varsort <- selectInput("sel_microagg_varsort", label=h5("Select variable for sorting"),
          choices=get_allNumericVars_name(), selected=input$sel_microagg_varsort, width="100%")
        out <- list(out, fluidRow(
          column(4, p(rb_measure, align="center")),
          column(4, p(sl_trim, align="center")),
          column(4, p(sel_varsort, align="center"))))
      } else {
        out <- list(out, fluidRow(
          column(6, p(rb_measure, align="center")),
          column(6, p(sl_trim, align="center"))))
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
    #if (is.null(input$sel_anon_continuous)) {
    #  return(NULL)
    #}
    m <- c("additive","correlated2","restr","ROMM","outdect")
    if (length(input$sel_noise_v) >=2 | length(get.sdcMicroObj(obj$sdcObj, type="numVars")) >= 2) {
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
      sel_method <- selectInput("sel_noise_method", label=h5("Select the Algorithm"),
        choices=choices_noise(),
        selected=input$sel_noise_method, width="100%")
      sel_method
    })
  })

  # variables selected
  output$ui_noise_vars <- renderUI({
    input$sel_noise_method
    isolate({
      lab <- h5("Select Variables")
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
    column(12, h4("Adding stochastic noise", align="center")),
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
    column(12, h4("Rank swapping", align="center")),
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
