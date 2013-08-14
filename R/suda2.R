setGeneric('suda2', function(obj, ...) {standardGeneric('suda2')})
setMethod(f='suda2', signature=c('sdcMicroObj'),
    definition=function(obj, ...) { 
      manipData <- get.sdcMicroObj(obj, type="manipKeyVars")
      keyVars <- colnames(manipData)
      
      risk <- get.sdcMicroObj(obj, type="risk")
      risk$suda2 <- suda2WORK(manipData,variables=keyVars,...)
      
      obj <- set.sdcMicroObj(obj, type="risk", input=list(risk))
      
      obj
    })
setMethod(f='suda2', signature=c("data.frame"),
    definition=function(obj, ...) { 
      suda2WORK(data=obj,...)
    })
setMethod(f='suda2', signature=c("matrix"),
    definition=function(obj, ...) { 
      suda2WORK(data=obj,...)
    })

suda2WORK <- function(data,variables=NULL,missing=-999,DisFraction=0.01){
  if(is.null(variables))
    variables <- colnames(data)
  dataX <- data[,variables,drop=FALSE]
  if(length(variables)==2)
    dataX <- cbind(dataX,rep(1,nrow(dataX)))
  else if(length(variables)==1)
    dataX <- cbind(dataX,rep(1,nrow(dataX)),rep(1,nrow(dataX)))
  for(i in 1:ncol(dataX)){
    if(!is.numeric(dataX[,i]))
      dataX[,i] <- as.numeric(dataX[,i])
  }
  dataX <- as.matrix(dataX)
  dataX[is.na(dataX)] <- missing
  #cat("Input data:\n")
  #print(head(dataX))
  dat <- .Call("Suda2",dataX,missing, ncol(dataX),DisFraction)$Res
  if(length(variables)==2)
    dat <- dat[,-3]
  else if(length(variables)==1)
    dat <- dat[,c(-2,-3)]
  colnames(dat) <- c(paste(variables,"_contribution",sep=""),"suda_score","dis_suda_score")
  res <- list("contributionPercent"=dat[,1:length(variables)],
      "score" = dat[,"suda_score"],
      "disScore" = dat[,"dis_suda_score"]
  )
  class(res) <- "suda2"
  if(length(variables)<=2)
    warning("This version of Suda2 can find MSUs only in Dataset with more than 2 variables,\n therefore dummy variables where added and the result might be wrong!")
  invisible(res)
}

print.suda2 <- function(x, ...){
  SEQ <- seq(0,0.7,0.1)+.Machine$double.eps
  DISSudaScore <- paste(">",seq(0.0,0.7,0.1))
  tab <- table(cut(x$disScore, breaks=c(-1,SEQ)))
  res <- data.frame("thresholds"=DISSudaScore,
      "number"=as.numeric(tab))
  cat("\n Dis suda scores table: \n")
  cat("- - - - - - - - - - - \n")
  print(res)
  cat(" - - - - - - - - - - - \n")
}
