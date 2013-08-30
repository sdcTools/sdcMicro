setGeneric('measure_risk', function(obj, ...) {standardGeneric('measure_risk')})
setMethod(f='measure_risk', signature=c('sdcMicroObj'),
    definition=function(obj, ...) { 
      origData <- get.sdcMicroObj(obj, type="origData")
      
      manipData <- get.sdcMicroObj(obj, type="manipKeyVars")    
      keyVars <- c(1:length(manipData))
      w <- get.sdcMicroObj(obj, type="weightVar")
      if(length(w)>0) {
        manipData <- cbind(manipData, origData[,w])
        w<-length(manipData)
      }else
        w <- NULL
      hhId <- get.sdcMicroObj(obj,type="hhId")
      if(length(hhId)>0) {
        manipData <- cbind(manipData, origData[,hhId])
        hhId<-length(manipData)
      }else
        hhId <- NULL
      res <- measure_riskWORK(manipData, keyVars, w=w, hid=hhId,...)
      risk <- get.sdcMicroObj(obj, type="risk")
      risk$global <- list()
      risk$global$risk <- res$global_risk
      risk$global$risk_ER <- res$global_risk_ER
      risk$global$risk_pct <- res$global_risk_pct
      risk$global$threshold <- res$global_threshold
      risk$global$max_risk <- res$max_global_risk
      if("hier_risk"%in%names(res)){
        risk$global$hier_risk_ER <- res$hier_risk_ER
        risk$global$hier_risk <- res$hier_risk
        risk$global$hier_risk_pct <- res$hier_risk_pct
      }
      risk$individual <- res$Res
      
      obj <- set.sdcMicroObj(obj, type="risk", input=list(risk))
      
      obj
    })
setMethod(f='measure_risk', signature=c("data.frame"),
    definition=function(obj, ...) { 
      measure_riskWORK(data=obj,...)
    })
setMethod(f='measure_risk', signature=c("matrix"),
    definition=function(obj, ...) { 
      measure_riskWORK(data=obj,...)
    })


measure_riskWORK <- function(data,keyVars,w=NULL,missing=-999,hid=NULL,max_global_risk=.01,fast_hier=TRUE){
  if(!is.data.frame(data))
    data <- as.data.frame(data)
  variables <- keyVars
  weight_variable <- w
  if((is.null(variables)||!variables%in%colnames(data))&&is.character(variables))
    stop("Please define valid key variables")
  else if(is.numeric(variables)){
    if(all(variables%in%c(1:ncol(data))))
      variables <- colnames(data)[variables]  
    else
      stop("Please define valid key variables")
  }
  if(!is.null(weight_variable)){
    if(!weight_variable%in%colnames(data)&&is.character(weight_variable))
      stop("Weight variable not found!")
    else if(is.numeric(weight_variable)){
      if(weight_variable%in%c(1:ncol(data)))
        weight_variable <- colnames(data)[weight_variable]
      else
        stop("Weight variable not found!")
    }
  }
  TFcharacter <- lapply(data[,keyVars,drop=FALSE],class)%in%"character"
  if(any(TFcharacter)){
    for(kvi in which(TFcharacter)){
      data[,keyVars[kvi]] <- as.factor(data[,keyVars[kvi]]) 
    }
  }
 
  f <- freqCalc(data,keyVars=keyVars,w=w)
  ir <- indivRisk(f,survey=!is.null(w))
  Res <- matrix(NA,ncol=3,nrow=nrow(data))
  Res[,1] <- ir$rk
  Res[,2] <- ir$fk
  Res[,3] <- f$Fk
  
  weighted <- 0
  if(!is.null(weight_variable))
    weighted <- 1
  n_key_vars <- length(variables)
  dataX <- data[,c(variables),drop=FALSE]
  for(i in 1:ncol(dataX)){
    if(!is.numeric(dataX[,i])){
      dataX[,i] <- as.numeric(dataX[,i])
  	}
  }
  if(weighted==1)
    dataX <-cbind(dataX,data[,weight_variable])
  dataX <- as.matrix(dataX)
  ind <- do.call(order,data.frame(dataX))
  dataX <- dataX[ind,,drop=FALSE]
  ind <- order(c(1:nrow(dataX))[ind])
  #res <- .Call("measure_risk",dataX,weighted,n_key_vars,2,-99,missing)#
  res <- list()
  res$Res <- Res
  colnames(res$Res) <- c("risk","fk","Fk")
  res$global_risk_ER <- sum(ir$rk, na.rm=TRUE)
  res$global_risk <- res$global_risk_ER/nrow(res$Res)
  res$global_risk_pct <- res$global_risk*100
  
  ind <- order(res$Res[,1],decreasing =TRUE)
  if(max_global_risk>=1 || max_global_risk<=0){
    stop("max_global_risk argument must be between 0 and 1!")
  }
  resth <- .Call("measure_threshold",res$Res[ind,1],max_global_risk)
  if(is.na(resth$global_threshold_unsafe)||is.na(resth$global_threshold_safe)||is.na(resth$global_threshold)){
    res[["global_threshold"]] <- NA
  }else if(resth$global_threshold_unsafe==resth$global_threshold&&resth$global_threshold_safe==resth$global_threshold)
    res[["global_threshold"]] <- NA
  else
    res[["global_threshold"]] <- resth$global_threshold
  res[["max_global_risk"]] <- max_global_risk
  class(res) <- "measure_risk"
  if(!is.null(hid)){
    ind <- order(data[,hid])
    dataX <- cbind(data[,hid],res$Res[,1])[ind,]
    ind <- order(c(1:nrow(dataX))[ind])
    for(i in 1:ncol(dataX)){
      if(!is.numeric(dataX[,i]))
        dataX[,i] <- as.numeric(dataX[,i])
    }
    dataX <- as.matrix(dataX)
    maxHH <- max(table(dataX[,1]) ,na.rm=TRUE)
    if(fast_hier){
      #warning("The households are to large for a fast computation of the hierachical risk.\n
      #(Use the parameter forceHier to perform the computation anyway)")
      reshier <- by(dataX[,2],dataX[,1],function(x)1-prod(1-x),simplify=TRUE)
      reshier <- data.frame(cbind(reshier,unique(dataX[,1])))
      colnames(reshier)=c("reshier","hhid")
      dataX <- data.frame(dataX[,1])
      colnames(dataX)=c("hhid")
      datX <- merge(dataX,reshier,all.x=TRUE)
      
      res$Res <- cbind(res$Res,datX$reshier[ind])
      res[["hier_risk_ER"]] <- sum(res$Res[,4], na.rm=TRUE)
      res[["hier_risk"]] <- sum(res$Res[,4], na.rm=TRUE)/nrow(res$Res)
      res[["hier_risk_pct"]] <- res[["hier_risk"]]*100
    }else{
      resh <- .Call("measure_hierachical",dataX)
      resh$Res <- resh$Res[ind]
      res$Res <- cbind(res$Res,resh$Res)
      res[["hier_risk_ER"]] <- resh[["hier_risk_ER"]]
      res[["hier_risk"]] <- resh[["hier_risk"]]
      res[["hier_risk_pct"]] <- resh[["hier_risk_pct"]]  
    }
    
    colnames(res$Res) <- c("risk","fk","Fk","hier_risk");
  }
  invisible(res)
}
setGeneric('ldiversity', function(obj, ldiv_index,l_recurs_c=2,missing=-999,...) {standardGeneric('ldiversity')})
setMethod(f='ldiversity', signature=c('sdcMicroObj'),definition=function(obj, ldiv_index,l_recurs_c=2,missing=-999) { 
      o <- obj@origData
      k <- obj@manipKeyVars
      n <- obj@manipNumVars
      s <- obj@manipStrataVar
      if(!is.null(k))
        o[,colnames(k)] <- k
      if(!is.null(n))
        o[,colnames(n)] <- n
      if(!is.null(s))
        o$sdcGUI_strataVar <- s
      kV <- colnames(obj@origData)[get.sdcMicroObj(obj,"keyVars")]
      obj@risk$ldiversity <- ldiversityWORK(data=o,keyVars=kV,l_recurs_c=l_recurs_c,ldiv_index=ldiv_index,missing=missing)
      return(obj)
    })
setMethod(f='ldiversity', signature=c("data.frame"),
    definition=function(obj, keyVars,ldiv_index,l_recurs_c=2,missing=-999) { 
      ldiversityWORK(data=obj,keyVars=keyVars,ldiv_index=ldiv_index,l_recurs_c=l_recurs_c,missing=missing)
    })
setMethod(f='ldiversity', signature=c("matrix"),
    definition=function(obj, keyVars,ldiv_index,l_recurs_c=2,missing=-999) { 
      ldiversityWORK(data=obj,keyVars=keyVars,ldiv_index=ldiv_index,l_recurs_c=l_recurs_c,missing=missing)
    })

      
ldiversityWORK <- function(data,keyVars,ldiv_index,missing=-999,l_recurs_c=2){
  variables <- keyVars
  if((is.null(variables)||!variables%in%colnames(data))&&is.character(variables))
    stop("Please define valid key variables")
  else if(is.numeric(variables)){
    if(all(variables%in%c(1:ncol(data))))
      variables <- colnames(data)[variables]  
    else
      stop("Please define valid key variables")
  }
  if(!is.null(ldiv_index)){
    if(is.numeric(ldiv_index)){
      ldiv_var <- colnames(data)[ldiv_index]
      ldiv_index <- length(variables)+1:length(ldiv_index)
    }else if(is.character(ldiv_index)){
      ldiv_var <- ldiv_index
      ldiv_index <- length(variables)+1:length(ldiv_index)
    }
    if(any(ldiv_var%in%variables))
      stop("Sensitivity variable should not be a keyVariable")
  }else
    ldiv_var <- character(0)
  
  n_key_vars <- length(variables)
  dataX <- data[,c(variables,ldiv_var),drop=FALSE]
  for(i in 1:ncol(dataX)){
    if(!is.numeric(dataX[,i]))
      dataX[,i] <- as.numeric(dataX[,i])
  }
  dataX <- as.matrix(dataX)
  ind <- do.call(order,data.frame(dataX))
  dataX <- dataX[ind,,drop=FALSE]
  ind <- order(c(1:nrow(dataX))[ind])
  if(is.null(ldiv_index))
    ldiv_index=-99
  if(length(ldiv_index)>5)
    stop("Maximal number of sensitivity variables is 5")
  res <- .Call("measure_risk",dataX,0,n_key_vars,l_recurs_c,ldiv_index,missing)
  res$Fk <- res$Res[,3]
  res$Res <- res$Res[ind,]
  if(all(ldiv_index!=-99)){
    res$Mat_Risk <- res$Mat_Risk[ind,]
    names(res)[names(res)=="Mat_Risk"] <- "ldiversity"
    colnames(res$ldiversity) <- c(paste(rep(ldiv_var,each=3),rep(c("Distinct_Ldiversity","Entropy_Ldiversity","Recursive_Ldiversity"),length(ldiv_index)),sep="_"),
        "MultiEntropy_Ldiversity","MultiRecursive_Ldiversity")
  }else{
    res <- res[names(res)!="Mat_Risk"]
  }
  # colnames(res$Res) <-  c("group_count","risk","fk")
  ind <- order(res$Res[,1],decreasing =TRUE)
  res <- res$ldiversity
  class(res) <- "ldiversity"
  invisible(res)
}
print.measure_risk <- function(x,...){
  cat("\n")
  cat("--------------------------\n")
  s <- sum(x$Res[,1] > median(x$Res[,1])+3*mad(x$Res[,1]) &  x$Res[,1] > 0.1)
  cat(paste(s,"obs. with higher risk as the main part\n"))
  cat("Expected no. of re-identifications:\n",round(x$global_risk_ER,2),"")
  cat("(",round(x$global_risk_pct,2),"%)\n")
  if(is.na(x$global_threshold))
    x$global_threshold <- Inf
  cat("Threshold:",round(x$global_threshold,2),"\n (for maximal global risk",round(x$max_global_risk,2),")\n")
  cat("--------------------------\n")
  if("hier_risk_ER"%in%names(x)){
    if(!is.na(x$hier_risk_ER)){
      cat("--------------------------\n")
      cat("Hierarchical risk \n")
      cat("--------------------------\n")
      cat("Expected no. of re-identifications:\n",round(x$hier_risk_ER,2),"")
      cat("(",round(x$hier_risk_pct,2),"% )\n")
      #print(round(summary(x$Res[,4]),2))
    }else{
      cat("--------------------------\n")
      cat("Hierarchical risk not available\n")
      cat("--------------------------\n")
    }
  }
}
print.ldiversity <- function(x,...){
  cat("--------------------------\n")
  cat("L-Diversity Measures \n")
  cat("--------------------------\n")
  print(summary(x[,grep("_Distinct_Ldiversity",colnames(x))]))
}
