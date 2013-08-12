setGeneric('localSupp', function(obj, threshold=0.15, keyVar,...) {standardGeneric('localSupp')})
setMethod(f='localSupp', signature=c('sdcMicroObj'),
    definition=function(obj, threshold=0.15, keyVar, ...) { 
      manipData <- get.sdcMicroObj(obj, type="manipKeyVars")
      keyVars <- colnames(manipData)
      rk <- get.sdcMicroObj(obj, type="risk")$individual[,1]
      TF <- rk>threshold
      if(!keyVar%in%colnames(manipData))
        stop("keyVar must be a defined categorical keyVariable!\n")
      manipData[TF,keyVar] <- NA
      obj <- nextSdcObj(obj)
      obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(manipData))
      a <- get.sdcMicroObj(obj, type="origData")[,get.sdcMicroObj(obj, type="keyVars")]
      b <- manipData
      w = which(is.na(a))
      r = w%%nrow(a)
      cc = ceiling(w/nrow(a))
      wb = which(is.na(b))
      rb = wb%%nrow(b)
      cb = ceiling(wb/nrow(b))
      d = data.frame(id=1:ncol(a), before=NA, after=NA)
      d[,2:3] <- t(sapply(1:ncol(a), function (x) c(sum(cc==x), sum(cb==x))))
      obj <- set.sdcMicroObj(obj, type="localSuppression", input=list(list(d$after-d$before)))
      
      obj <- calcRisks(obj)
      
      obj
    })
setMethod(f='localSupp', signature=c("ANY"),
    definition=function(obj, threshold=0.15,keyVar,indivRisk) { 
      ## x ... object from class freqCalc
      ## keyVar ... variables used for local suppression, ordered
      ## indivRisk ... vector of individual risks
      ## fixme: better method for local suppression
      ## calculate risk a second (and third time) and choose another keyVar! no
      # keyVars = x$keyVars + 1  ## indexG is now first
      if( class(obj) != "freqCalc" )
        stop("obj is not from class freqCalc")
      obj$freqCalc[indivRisk > threshold, keyVar[1]] <- NA
      obj
    })

