# return sdcObj
sdcObj <- reactive({
  if (is.null(obj$sdcObj)) {
    return(NULL)
  }
  obj$sdcObj
})


inputdata <- reactive({
  if(is.null(obj$inputdata)) {
    return(NULL)
  }
  return(obj$inputdata)
})

# increment reactive variable 'setupval_inc' by one once
# obj$inputdata has changed
# this is required for the setupTable to be refreshed because
# all inputBindings also have 'setupval_inc' in their id
observeEvent(obj$inputdata, {
  if (!is.null(obj$inputdata)) {
    obj$setupval_inc <- obj$setupval_inc + 1
  }
})

comptime <- reactive({
  z <- as.difftime(obj$comptime, units="secs")

  if (obj$comptime <60) {
    return(paste(round(as.numeric(z, units="secs"), digits=2),"seconds"))
  } else {
    return(paste(round(as.numeric(z, units="mins"), digits=2),"minutes"))
  }
})

# which anonymization steps have been applied?
anonPerformed <- reactive({
  res <- obj$anon_performed
  if (is.null(res)) {
    return(NULL)
  }
  if (length(res)==0) {
    return(NULL)
  }
  res
})

# all numeric variables in inputdata
numVars <- reactive({
  inp <- inputdata()
  if (is.null(inp)) {
    return(NULL)
  }
  names(inp)[sapply(inp, class)%in% c("numeric","integer")]
})

# all factor variables in inputdata
facVars <- reactive({
  inp <- inputdata()
  if (is.null(inp)) {
    return(NULL)
  }
  names(inp)[sapply(inp, class) == c("factor")]
})

# all character variables in inputata
charVars <- reactive({
  inp <- inputdata()
  if (is.null(inp)) {
    return(NULL)
  }
  names(inp)[sapply(inp, class) == c("character")]
})

# all possible stratification variables
possStrataVars <- reactive({
  setdiff(facVars(), get_keyVars_names())
})

# all variables available in the input data set
allVars <- reactive({
  inp <- inputdata()
  if (is.null(inp)) {
    return(NULL)
  }
  cn <- colnames(inp)
  cl <- sapply(1:ncol(inp), function(x) {
    class(inp[[x]])
  })
  names(cn) <- paste0(cn," (",cl,")")
  cn
})

# all variables in the sdcProblem (after variables have been possibly deleted)
allVarsP <- reactive({
  colnames(get_origData())
})

dataTypes <- reactive({
  inputdata <- inputdata()
  if (is.null(inputdata)) {
    return(NULL)
  }
  cn <- colnames(inputdata)
  cl <- sapply(1:ncol(inputdata), function(x) {
    class(inputdata[[x]])
  })
  cl
})

# index of categorical key variables
get_keyVars <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(curObj@keyVars)
})

# categorical key variables by names
get_keyVars_names <- reactive({
  if (is.null(sdcObj())) {
    return(NULL)
  }
  return(colnames(get_origData())[get_keyVars()])
})

# index of weight-variable
get_weightVar <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(curObj@weightVar)
})

# weightVar by name
get_weightVar_name <- reactive({
  if (is.null(sdcObj())) {
    return(NULL)
  }
  return(colnames(get_origData())[get_weightVar()])
})

# index of numerical key variables
get_numVars <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(curObj@numVars)
})

# get numerical key-variables by names
get_numVars_names <- reactive({
  if (is.null(sdcObj())) {
    return(NULL)
  }
  return(colnames(get_origData())[get_numVars()])
})

# index of pram variables
get_pramVars <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(curObj@pramVars)
})
# get pram variables by names
get_pramVars_names <- reactive({
  if (is.null(sdcObj())) {
    return(NULL)
  }
  return(colnames(get_origData())[get_pramVars()])
})

# we allow all key vars (they are factors) and the pram vars that exist in
# the current problem (which are any non-used factor variables) and do not show
# those that already have been pramed once!
pramVars <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(get_pramVars_names())
})

get_strataVar <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(get.sdcMicroObj(curObj, "strataVar"))
})
# strataVar by name
get_strataVar_names <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  sv <- get_strataVar()
  if (is.null(sv)) {
    return(NULL)
  }
  return(colnames(get_origData())[sv])
})

# all possible stratification variables
poss_strataVarP <- reactive({
  setdiff(allVarsP(), c(get_all_numericvars_name()))
})

# original data
get_origData <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(curObj@origData)
})

get_manipKeyVars <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(curObj@manipKeyVars)
})

get_manipNumVars <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(curObj@manipNumVars)
})

get_manipPramVars <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(curObj@manipPramVars)
})

# risks
get_risk <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  return(as.data.frame(curObj@risk$individual))
})

get_all_factorvars_name <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  tmp <- get.sdcMicroObj(curObj, type="origData")
  names(tmp)[sapply(tmp, class)%in% c("factor")]
})
# all numeric/integer variables
get_all_integervars_name <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  tmp <- get.sdcMicroObj(curObj, type="origData")
  names(tmp)[sapply(tmp, class)%in% c("integer")]
})
get_all_numericvars_name <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  tmp <- get.sdcMicroObj(curObj, type="origData")
  names(tmp)[sapply(tmp, class)%in% c("numeric")]
})

get_allNumericVars_name <- reactive({
  c(get_all_numericvars_name(), get_all_integervars_name())
})

# possible variables for microaggregation and addNoise,...
# excluding sampling weights, strata-variables, ghostVars,..
possvars_numericmethods <- reactive({
  if (is.null(sdcObj())) {
    return(NULL)
  }
  get_numVars_names()
})

# returns list with choices for possible variables while setting up the sdcProblem
# sdcVars <- reactive({
#   allVars <- colnames(obj[["inputdata"]])
#   out <- list()
#   out$kv <- c("",setdiff(facVars(), c(input$sel_nV,input$sel_wV,input$sel_hhID,input$sel_strataV)))
#   out$nv <- c("",setdiff(numVars(), c(input$sel_kV,input$sel_wV,input$sel_hhID,input$sel_strataV)))
#   out$wv <- c("(do not use sampling weights)"="none",setdiff(numVars(), c(input$sel_kV,input$sel_nV,input$sel_hhID,input$sel_strataV)))
#   out$hhid <- c("(do not use household-identification variable)"="none",setdiff(allVars(), c(input$sel_kV,input$sel_nV,input$sel_wV,input$sel_strataV)))
#   out$strataV <- c("(do not use stratification variable)"="none",setdiff(facVars(), c(input$sel_kV,input$sel_nV,input$sel_wV,input$sel_hhID)))
#   out
# })

# calculates all variables that might be removed
# all variables that are not listed as key variables, strata, weights,..
# possRemoveVars <- reactive({
#   if (is.null(input$sel_kV)) {
#     return(NULL)
#   }
#   allVars <- colnames(obj[["inputdata"]])
#   exVars <- unique(c(input$sel_kV, input$sel_nV, input$sel_wV, input$sel_hhID, input$sel_strataV))
#   setdiff(allVars, exVars)
# })

# calculates the variables that might be used as ghost-vars
possGhostVars <- reactive({
  if (is.null(sdcObj())) {
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
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  extractManipData(curObj, randomizeRecords=input$rb_export_randomizeorder)
})

# compute some risk-measures
measure_riskComp <- reactive({
  if (is.null(sdcObj())) {
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

## information on local suppression
info_localsupp <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  curObj@localSuppression$totalSupps
})

## information on current sdcProblem used in sidebars
infodat <- reactive({
  # important variables
  kV <- get_keyVars_names()
  if (length(kV)==0 ) {
    return(NULL)
  }
  df <- data.frame(Variable=kV, type="cat. key variable", Suppressions=0)

  ls <- info_localsupp()
  if (!is.null(ls)) {
    df$Suppressions <- as.integer(ls[1,])
  }

  nV <- get_numVars_names()
  if (length(nV)>0) {
    df <- rbind(df, data.frame(Variable=nV, type="num. key variable", Suppressions=NA))
  }
  wV <- get_weightVar_name()
  if (length(wV)>0) {
    df <- rbind(df, data.frame(Variable=wV, type="sampling weight", Suppressions=NA))
  }
  sV <- get_strataVar_names()
  if (length(sV)>0) {
    df <- rbind(df, data.frame(Variable=sV, type="strata", Suppressions=NA))
  }

  pV <- get_pramVars_names()
  if (length(pV)>0) {
    df <- rbind(df, data.frame(Variable=pV, type="PRAM variable", Suppressions=NA))
  }

  # params
  res <- obj$sdcObj@options
  params <- data.frame(Parameter=c("number of records","alpha","random seed"),
    Value=c(nrow(obj$inputdata), res$alpha,res$seed))
  params$Value <- as.character(params$Value)
  list(df=df, params=params)
})

# return value labels from stata (if any)
stataLabs <- reactive({
  obj$stata_labs
})


# perform calculation of l-diversity using data.frame-method
calc_ldiv_result <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }

  inpdat <- extractManipData(curObj, randomizeRecords="no")
  keyVars <- get_keyVars_names()
  sensVars <- input$ldiv_sensvar
  ldiv <- ldiversity(obj=inpdat, keyVars=keyVars, ldiv_index=sensVars, l_recurs_c=input$ldiv_recconst, missing=NA)
  risk <- curObj@risk

  # table with violating observations
  ldiv2 <- ldiv[,grep("_Distinct_Ldiversity",colnames(ldiv)),drop=FALSE]
  for (i in 1:ncol(ldiv2)) {
    ss <- summary(ldiv2[,i])
    if (i==1) {
      df <- data.frame(stats=names(ss), value=as.numeric(ss))
    } else {
      df <- cbind(df, data.frame(as.numeric(ss)))
    }
  }
  colnames(df) <- c("stats", colnames(ldiv2))
  fk <- risk$individual[,2]
  TFfk <- apply(ldiv2,1,function(x)any(x<input$ldiv_recconst))
  if (!any(TFfk)) {
    tab <- data.frame()
  } else {
    tab <- cbind(ldiv2[TFfk,],fk[TFfk],inpdat[TFfk,])
    colnames(tab)[1:ncol(ldiv2)] <- colnames(ldiv2)
    colnames(tab)[ncol(ldiv2)+1] <- "fk"
    tab <- tab[order(tab[,1]),]
  }
  return(list(tab=tab, df=df))
})

# current result of ldiversity-calculation
get_ldiv_result <- reactive({
  obj$ldiv_result
})

# calculate suda2-measure using data.frame-method
calc_suda2_result <- reactive({
  curObj <- sdcObj()
  if (is.null(curObj)) {
    return(NULL)
  }
  inpdat <- extractManipData(curObj, randomizeRecords="no")
  keyVars <- get_keyVars_names()
  suda2 <- suda2(obj=inpdat, variables=keyVars, missing=NA, DisFraction=input$suda2_disf)

  SEQ <- seq(0, 0.7, 0.1) + .Machine$double.eps
  DISSudaScore <- c("== 0", "(0.0, 0.1]","(0.1, 0.2]", "(0.2, 0.3]", "(0.3, 0.4]", "(0.4, 0.5]", "(0.5, 0.6]", "(0.6, 0.7]","> 0.7")
  tab <- table(cut(suda2$disScore, breaks = c(-1, SEQ, Inf)))
  df_thresholds <- data.frame(interval = DISSudaScore, "number of records" = as.integer(tab))
  colnames(df_thresholds) <- c("Interval", "Number of records")
  return(list(thresholds=df_thresholds, attribute_contributions=suda2$attribute_contributions, DisFraction=input$suda2_disf))
})

# current result of suda2-calculation
get_suda2_result <- reactive({
  obj$suda2_result
})
