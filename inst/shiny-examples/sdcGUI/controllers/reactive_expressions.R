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

dataTypes <- reactive({
  cn <- colnames(obj[["inputdata"]])
  cl <- sapply(1:ncol(obj$inputdata), function(x) {
    class(obj$inputdata[[x]])
  })
  cl
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

# weightVar by name
get_weightVar_name <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  return(colnames(get_origData())[get_weightVar()])
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

# returns list with choices for possible variables while setting up the sdcProblem
sdcVars <- reactive({
  allVars <- colnames(obj[["inputdata"]])
  out <- list()
  out$kv <- c("",setdiff(facVars(), c(input$sel_nV,input$sel_wV,input$sel_hhID,input$sel_strataV)))
  out$nv <- c("",setdiff(numVars(), c(input$sel_kV,input$sel_wV,input$sel_hhID,input$sel_strataV)))
  out$wv <- c("(do not use sampling weights)"="none",setdiff(numVars(), c(input$sel_kV,input$sel_nV,input$sel_hhID,input$sel_strataV)))
  out$hhid <- c("(do not use household-identification variable)"="none",setdiff(allVars(), c(input$sel_kV,input$sel_nV,input$sel_wV,input$sel_strataV)))
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


# compute some risk-measures
measure_riskComp <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }

  bm <- 0.1
  res <- list()
  risk <- obj$sdcObj@risk
  originalRisk <- obj$sdcObj@originalRisk
  res$s <- sum((risk$individual[,1] > median(risk$individual[,1])+2*mad(risk$individual[,1])) & (risk$indiviual[,1] > bm))
  res$sorig <- sum((originalRisk$individual[,1] > median(originalRisk$individual[,1])+2*mad(originalRisk$individual[,1])) & (originalRisk$indiviual[,1] > bm))
  res$benchmark <- bm

  res$exp_reident_m <- round(risk$global$risk_ER,2)
  res$exp_reident_mp <- round(risk$global$risk_pct,2)
  res$exp_reident_o <- round(originalRisk$global$risk_ER,2)
  res$exp_reident_op <- round(originalRisk$global$risk_pct,2)

  res$hierrisk <- FALSE
  if ("hier_risk_ER"%in%names(risk$global)) {
    if (!is.na(risk$global$hier_risk_ER)) {
      res$hierrisk <- TRUE
      res$hier_exp_m <- round(risk$global$hier_risk_ER,2)
      res$hier_exp_mp <- round(risk$global$hier_risk_pct,2)
      res$hier_exp_o <- round(originalRisk$global$hier_risk_ER,2)
      res$hier_exp_op <- round(originalRisk$global$hier_risk_pct,2)
    } else {
      res$hier <- NA
    }
  } else {
  }
  res
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



# at least one categorical key variable
# max 1 weight-variable
# max 1 household-id
# check, if sdcProblem can be used, if not show error message!
check_setup_btn <- reactive({
  nc <- length(allVars())
  if (!is.null(input$rb_keyVars_1)) {
    res_kv <- sapply(1:nc, function(i) {
      as.character(input[[paste0("rb_keyVars_",i)]])[1]
    })

    # at least one categorical key variable
    ind_catkv <- which(res_kv=="categorical")
    if (length(ind_catkv) < 1) {
      return(-1) # at least one categorical key variable required
    }

    # sampling weights
    res_w <- sapply(1:nc, function(i) {
      input[[paste0("cb_setup_weight",i)]]
    })
    ind_w <- which(res_w==TRUE)
    if (length(ind_w) > 1) {
      return(-2) # more than one variable selected as weight-variable
    }
    # if selected, it cant be a categorical key variable
    if (length(ind_w)==1) {
      if (res_kv[ind_w]=="categorical") {
        return(-3) # selected weight-variable must not be a categorical key variable
      }
      if (res_kv[ind_w]=="numerical") {
        return(-4) # selected weight-variable must not be a numerical key variable
      }
    }
    # household ids
    res_h <- sapply(1:nc, function(i) {
      input[[paste0("cb_setup_household",i)]]
    })
    ind_h <- which(res_h==TRUE)
    if (length(ind_h) > 1) {
      return(-5) # more than one variable selected as hhid-variable
    }
    # if selected, it cant be a key variable
    if (length(ind_h)==1) {
      if (res_kv[ind_h]=="categorical") {
        return(-6) # selected hhid must not be a categorical key variable
      }
      if (res_kv[ind_h]=="numerical") {
        return(-7) # selected hhid must not be a numerical key variable
      }
    }

    # strata
    res_s <- sapply(1:nc, function(i) {
      input[[paste0("cb_setup_strata",i)]]
    })
    ind_s <- which(res_s==TRUE)
    # any variables selected as stratas must not be used as key variables
    if (length(ind_s)>0) {
      if ("categorical" %in% res_kv[ind_s]) {
        return(-8) # at least one selected stratification variable is also a categorical key-var
      }
      if ("numerical" %in% res_kv[ind_s]) {
        return(-9) # at least one selected stratification variable is also a numerical key-var
      }
    }

    # deleted variables
    res_d <- sapply(1:nc, function(i) {
      input[[paste0("cb_setup_delete",i)]]
    })
    ind_d <- which(res_d==TRUE)
    if (length(ind_d)>0) {
      if ("categorical" %in% res_kv[ind_d]) {
        return(-10) # key variables cannot be deleted!
      }
      if ("numerical" %in% res_kv[ind_d]) {
        return(-10) # key variables cannot be deleted!
      }

      used_vars <- sort(unique(c(ind_w, ind_h, ind_s)))
      if (length(used_vars)>0) {
        used_vars <- sort(unique(used_vars))
        if (length(intersect(used_vars, ind_d)) > 0) {
          return(-11) # variables used as weights, household_ids or strata must not be deleted
        }
      }
    }
  }
  return(0)
})


