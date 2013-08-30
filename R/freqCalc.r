`freqCalc` <-
    function(x, keyVars, w=NULL, fast=TRUE){

#  classInfo <- character()
#  xKeys <- x[,keyVars,drop=FALSE]
#  for(i in 1:ncol(xKeys)){
#    classInfo[i] <- class(xKeys[,i])
#  }
#  dfInfo <- is.data.frame(x)
#  ## internally code as numbers:
#  for(i in 1:ncol(xKeys)){
#	  xKeys[,i] <- as.numeric(as.factor(xKeys[,i]))
#  }
  ## TODO: directly work with xKeys in ffc and freqCalc
 #x[,keyVars] <- xKeys
  if(fast){
    TFna <- any(is.na(x[,keyVars]))
    if(TFna)
      z <- ffc(x,keyVars,w)
    else 
      z <- sffc(x,keyVars,w)
#    if(dfInfo) z$freqCalc <- data.frame(z$freqCalc)
#    if(any(classInfo == "factor")){
#      a <- which(classInfo=="factor")
#      for(i in a){
#        z$freqCalc[,i] <- as.factor(z$freqCalc[,i])
#      }
#    }
  }else{
    #x <- apply(x[,keyVars], 2, function(x) { as.integer(as.factor(x))})
    #x <- as.matrix(x)
    y <- x	
    
    x <- x[,keyVars]
    x <- apply(x, 2, function(x) { as.integer(as.factor(x))})
    #x <- apply(x, 2, function(x) { as.integer(as.factor(x))})
    x <- apply(x, 1, rbind)
    
    N <- dim(y)[1]
    S <- dim(y[,keyVars,drop=FALSE])[2]
    res <- .C(	"f2",
        as.integer(c(N,S)),
        as.integer(ifelse(is.na(x), -999999, x)),
        as.integer(rep(0,N)),
        as.numeric(rep(0.0, N)),
        as.numeric(if(length(w)==0) rep(1,N) else y[,w]),
        PACKAGE="sdcMicro", NUOK=TRUE)
#    if(dfInfo) y <- data.frame(y)
#    if(any(classInfo == "factor")){
#      a <- which(classInfo=="factor")
#      for(i in a){
#        y[,i] <- as.factor(y[,i])
#      }
#    }
    z <- list(freqCalc=y, keyVars=keyVars, w=w, indexG=NULL, fk=res[[3]], Fk=res[[4]], n1=length(which(res[[3]]==1)), n2=length(which(res[[3]]==2)))
    class(z) <- "freqCalc" 
  }
  invisible(z)
}

ffc <- function(x, keyVars, w = NULL) {
  treatmissing <- -999
  dataX <- x[,keyVars,drop=FALSE]
  weighted <- 0
  if(!is.null(w)){
    weighted <- 1
    dataX <- cbind(dataX,x[,w])
  }
  for(i in 1:ncol(dataX)){
    if(!is.numeric(dataX[,i]))
      dataX[,i] <- as.numeric(dataX[,i])
  }
  dataX <- as.matrix(dataX)
  while(any(dataX==treatmissing,na.rm=TRUE)){
	  treatmissing <- -sample(999:999999,1)
  }  
  dataX[is.na(dataX)] <- treatmissing
  ind <- do.call(order,data.frame(dataX))
  dataX <- dataX[ind,,drop=FALSE]
  ind <- order(c(1:nrow(dataX))[ind])
  if(weighted==1){
    Res <- .Call("ffc",dataX,1,length(keyVars),treatmissing)$Res[ind,]
    Fk <- Res[,2]
    fk <- Res[,1]
  }else
    Fk <- fk <- .Call("ffc",dataX,0,length(keyVars),treatmissing)$Res[ind,1]
  res <- list(
      freqCalc = x,
      keyVars = keyVars,
      w = w,
      indexG = NULL,
      fk = as.integer(fk),
      Fk = Fk,
      n1 = length(which(fk==1)),
      n2 = length(which(fk==2))
  )
  class(res) <- "freqCalc"
  invisible(res)
}

sffc <- function(x, keyVars, w = NULL) {
  xorig <- x
  x$idvarextraforsffc=1:nrow(x)
  if(is.numeric(keyVars))
    keyVars <- colnames(x)[keyVars]
  if(is.null(w)){
    dat <- data.table(x[,c(keyVars,"idvarextraforsffc")])
    setkeyv(dat,keyVars)
    erg <- vector()
    cmd <- paste("ergO <- dat[,list(fk=.N),by=list(",paste(keyVars,collapse=","),")]",sep="")
    eval(parse(text=cmd))
    erg <- merge(ergO,dat)
    setkey(erg,idvarextraforsffc)
    res <- list(
        freqCalc = xorig,
        keyVars = keyVars,
        w = w,
        indexG = NULL,
        fk = as.integer(erg$fk),
        Fk = as.numeric(erg$fk),
        n1 = length(which(erg$fk==1)),
        n2 = length(which(erg$fk==2))
    )
  }else{
    dat <- data.table(x[,c(keyVars,"idvarextraforsffc")],weight=x[,w])
    setkeyv(dat,keyVars)
    erg <- vector()
    cmd <- paste("erg <- dat[,list(Fk=sum(weight),fk=.N),by=list(",paste(keyVars,collapse=","),")]",sep="")
    eval(parse(text=cmd))
    erg <- merge(erg,dat)
    setkey(erg,idvarextraforsffc)
    res <- list(
        freqCalc = xorig,
        keyVars = keyVars,
        w = w,
        indexG = NULL,
        fk = as.integer(erg$fk),
        Fk = as.numeric(erg$Fk),
        n1 = length(which(erg$fk==1)),
        n2 = length(which(erg$fk==2))
    )
  }
  class(res) <- "freqCalc"
  invisible(res)
}
  
