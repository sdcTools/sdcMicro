# all numeric variables in inputdata
numVars <- reactive({
  tmp <- obj$inputdata
  names(tmp)[sapply(tmp, class)%in% c("numeric","integer")]
})

# all factor variables in inputdata
facVars <- reactive({
  tmp <- obj$inputdata
  names(tmp)[sapply(tmp, class) == c("factor")]
})

# all possible stratification variables
possStrataVars <- reactive({
  setdiff(facVars(), get_keyVars_names())
})

# all variables available in the input data set
allVars <- reactive({
  cn <- colnames(obj[["inputdata"]])
  cl <- sapply(1:ncol(obj$inputdata), function(x) {
    class(obj$inputdata[[x]])
  })
  names(cn) <- paste0(cn," (",cl,")")
  cn
})

# index of categorical key variables
get_keyVars <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(obj$sdcObj@keyVars)
})

# categorical key variables by names
get_keyVars_names <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(colnames(get_origData())[get_keyVars()])
})

# index of weight-variable
get_weightVar <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(obj$sdcObj@weightVar)
})

# index of numerical key variables
get_numVars <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(obj$sdcObj@numVars)
})

# get numerical key-variables by names
get_numVars_names <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(colnames(get_origData())[get_numVars()])
})

get_strataVar <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(get.sdcMicroObj(obj$sdcObj, "strataVar"))
})
# strataVar by name
get_strataVar_names <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  sv <- get_strataVar()
  if (is.null(sv)) {
    return(NULL)
  }
  return(colnames(get_origData())[sv])
})

# original data
get_origData <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(obj$sdcObj@origData)
})

get_manipKeyVars <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(obj$sdcObj@manipKeyVars)
})

get_manipNumVars <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(obj$sdcObj@manipNumVars)
})

# risks
get_risk <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(as.data.frame(obj$sdcObj@risk$individual))
})

# all numeric/integer variables
get_allNumericVars_name <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  tmp <- get.sdcMicroObj(obj$sdcObj, type="origData")
  names(tmp)[sapply(tmp, class)%in% c("numeric","integer")]
})

# possible variables for microaggregation and addNoise,...
# excluding sampling weights, strata-variables, ghostVars,..
possvars_numericmethods <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }

  get_numVars_names()

  # all numeric variables
  #numVars <- get_allNumericVars_name()

  # sampling weights should never be microaggregated
  #tmp <- get.sdcMicroObj(obj$sdcObj, type="origData")
  #kV <- get.sdcMicroObj(obj$sdcObj, type="keyVars")
  #wV <- get.sdcMicroObj(obj$sdcObj, type="weightVar")
  #hhId <- get.sdcMicroObj(obj$sdcObj, type="hhId")
  #strataVar <- get.sdcMicroObj(obj$sdcObj, type="strataVar")
  #ghostVars <- get.sdcMicroObj(obj$sdcObj, type="ghostVars")
  #ghostVars <- unlist(lapply(ghostVars, function(x) {
  #  x[[2]]
  #}))
  #non_poss <- c(kV,wV,hhId,strataVar,ghostVars)
  #setdiff(numVars, c(names(tmp)[non_poss]))
})


# choices for dropdown-menus in Tab 'anonymize'
res_choices_anon <- reactive({
  if (is.null(input$sel_anonymize)) {
    return(NULL)
  }
  if (!input$sel_anonymize %in% c("manage_sdcProb","cat_anon","cat_num")) {
    return(NULL)
  }
  if (input$sel_anonymize=="manage_sdcProb") {
    choices <- c(
      "Show Summary"="sdcObj_summary",
      "Add 'Ghost'-Variables"="sdcObj_addghostvars",
      "Reset sdcProblem"="sdcObj_reset")
  }
  if (input$sel_anonymize=="cat_anon") {
    choices <- c(
      "Postrandomization (PRAM)"="pram",
      "Recoding"="recode",
      "k-Anonymity"="kanon",
      "Supress values with high risks"="supp_threshold")
  }
  if (input$sel_anonymize=="cat_num") {
    choices <- c(
      "Apply Microaggregation"="microaggregation",
      "Adding Noise to the data"="noise",
      #"Shuffling the data"="shuffling",
      "Apply Rank-Swapping"="rankswap")
  }
  choices
})

# returns list with choices for possible variables while setting up the sdcProblem
sdcVars <- reactive({
  allVars <- colnames(obj[["inputdata"]])
  out <- list()
  out$kv <- c("",setdiff(facVars(), c(input$sel_nV,input$sel_wV,input$sel_hhID,input$sel_strataV)))
  out$nv <- c("",setdiff(numVars(), c(input$sel_kV,input$sel_wV,input$sel_hhID,input$sel_strataV)))
  out$wv <- c("(do not use sampling weights)"="none",setdiff(numVars(), c(input$sel_kV,input$sel_nV,input$sel_hhID,input$sel_strataV)))
  out$hhid <- c("(do not use household-identification variable)"="none",setdiff(facVars(), c(input$sel_kV,input$sel_nV,input$sel_wV,input$sel_strataV)))
  out$strataV <- c("(do not use stratification variable)"="none",setdiff(facVars(), c(input$sel_kV,input$sel_nV,input$sel_wV,input$sel_hhID)))
  out
})

# calculates all variables that might be removed
# all variables that are not listed as key variables, strata, weights,..
possRemoveVars <- reactive({
  if (is.null(input$sel_kV)) {
    return(NULL)
  }
  allVars <- colnames(obj[["inputdata"]])
  exVars <- unique(c(input$sel_kV, input$sel_nV, input$sel_wV, input$sel_hhID, input$sel_strataV))
  setdiff(allVars, exVars)
})

# calculates the variables that might be used as ghost-vars
possGhostVars <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  out <- list(kv=c(), gv=c())

  xx <- obj$sdcObj
  cn <- colnames(get.sdcMicroObj(xx, type="origData"))

  kv <- cn[get.sdcMicroObj(xx, type="keyVars")]
  nv <- cn[get.sdcMicroObj(xx, type="numVars")]
  pv <- cn[get.sdcMicroObj(xx, type="pramVars")]
  wv <- cn[get.sdcMicroObj(xx, type="weightVar")]
  hhid <- cn[get.sdcMicroObj(xx, type="hhId")]
  sv <- cn[get.sdcMicroObj(xx, type="strataVar")]
  non_poss <- c(kv, nv, pv, wv, hhid, sv)
  gv <- get.sdcMicroObj(xx, type="ghostVars")
  if (!is.null(gv)) {
    ex_gv <- cn[unlist(sapply(gv, function(x) {
      x[[2]]
    }))]
    non_poss <- c(non_poss, ex_gv)
  }
  non_poss <- unique(non_poss)
  out$kv <- kv
  out$gv <- setdiff(cn, non_poss)
  out
})

# the (anonymized) dataset that will be written to a file
exportData <- reactive({
  extractManipData(obj$sdcObj)
})


### generalized test-function to create uiOutput
## selectInput
ui_custom_selectInput <- function(choices, id, label, multiple=FALSE) {
  if (is.null(input[[id]])) {
    sel <- ""
  } else {
    sel <- input[[id]]
  }
  renderUI({selectInput(inputId=id, label=h5(label), choices=choices, selected=sel, multiple=multiple, width="100%")})
}
## numericInput
ui_custom_numericInput <- function(id, label, min, max) {
  if (is.null(input[[id]])) {
    sel <- min
  } else {
    sel <- input[[id]]
  }
  renderUI({numericInput(inputId=id, label=h5(label), min=min, max=max, value=sel, width="100%")})
}

ui_custom_textInput <- function(id, label, placeholder="please enter a text") {
  isolate({
    if (is.null(input[[id]])) {
      sel <- ""
    } else {
      sel <- input[[id]]
    }
    renderUI({textInput(inputId=id, label=h5(label), value=sel, width="100%", placeholder=placeholder)})
  })
}
