setGeneric('rankSwap', function(obj, variables=NULL,TopPercent=5,BottomPercent=5,
        K0=-1,R0=.95,P=0,missing=-999,seed=NULL){standardGeneric('rankSwap')})
setMethod(f='rankSwap', signature=c('sdcMicroObj'),
    definition=function(obj, variables=NULL,TopPercent=5,BottomPercent=5,K0=-1,R0=.95,P=0,missing=-999,seed=NULL) { 
      manipData <- get.sdcMicroObj(obj, type="manipNumVars")
      
      if(is.null(variables)){
        variables <- colnames(manipData)
      } 
      
      res <- rankSwapWORK(manipData, variables=variables,TopPercent=TopPercent,BottomPercent=BottomPercent,
          K0=K0,R0=R0,P=P,missing=missing,seed=seed)    
      
      obj <- nextSdcObj(obj)
      obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(res))
      obj <- dRisk(obj)
      obj <- dUtility(obj)
      
      obj
    })
setMethod(f='rankSwap', signature=c("data.frame"),
    definition=function(obj, variables=NULL,TopPercent=5,BottomPercent=5,K0=-1,R0=.95,P=0,missing=-999,seed=NULL) { 
      rankSwapWORK(data=obj,variables=variables,TopPercent=TopPercent,BottomPercent=BottomPercent,K0=K0,R0=R0,
          missing=missing,seed=seed)
    })
setMethod(f='rankSwap', signature=c("matrix"),
    definition=function(obj, variables=NULL,TopPercent=5,BottomPercent=5,K0=-1,R0=.95,P=0,missing=-999,seed=NULL) { 
      rankSwapWORK(data=obj,variables=variables,TopPercent=TopPercent,BottomPercent=BottomPercent,K0=K0,R0=R0,
          missing=missing,seed=seed)
    })

rankSwapWORK <- function(data,variables=NULL,TopPercent=5,BottomPercent=5,K0=-1,R0=.95,P=0,missing=-999,seed=NULL){
  if(is.null(variables)){
    if(is.matrix(data))
      variables <- 1:ncol(data)
    else
      variables <- colnames(data)
  }
  dataX <- data[,variables]
  dataX <- as.matrix(dataX)
  
  if ( !all(apply(dataX, 2, is.numeric)) ) {
    dataX <- apply(dataX, 2, as.numeric)
  }
  
  data2 <- dataX                 
  dataX[is.na(dataX)] <- missing
  data2[,] <- NA                                                                   
  if(is.null(seed))
    seed <- -1
  seed <- as.integer(seed)
  dat <- .Call("RankSwap",dataX,data2,missing,TopPercent,BottomPercent,K0,R0,P,seed)$Res                                                                                                             
  data[,variables] <- dat
  invisible(data)
}
