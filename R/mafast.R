setGeneric('mafast', function(obj, variables=NULL,by=NULL,aggr=3,measure=mean) {standardGeneric('mafast')})
setMethod(f='mafast', signature=c('sdcMicroObj'),
    definition=function(obj, variables=NULL,by=NULL,aggr=3,measure=mean) { 
      x <- get.sdcMicroObj(obj, type="manipNumVars")
      if(is.null(by)){
        if(!is.null(obj@strataVar))
          by <- colnames(obj@origData)[obj@strataVar]
      }
      if(is.null(variables))
        variables <- colnames(obj@origData)[obj@numVars]
      res <- mafastWORK(x, variables=variables,by=by,aggr=aggr,measure=measure)
      obj <- nextSdcObj(obj)
      obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(as.data.frame(res)))
      obj <- dRisk(obj)
#      obj <- dRiskRMD(obj)
      obj <- dUtility(obj)
      
      obj
    })
setMethod(f='mafast', signature=c("data.frame"),
    definition=function(obj, variables=NULL,by=NULL,aggr=3,measure=mean){
      mafastWORK(x=obj,variables=variables,by=by,aggr=aggr,measure=measure)
    })
setMethod(f='mafast', signature=c("matrix"),
    definition=function(obj, variables=NULL,by=NULL,aggr=3,measure=mean){ 
      mafastWORK(x=obj,variables=variables,by=by,aggr=aggr,measure=measure)
    })
mafastWORK <- function(x,variables=colnames(x),by=NULL,aggr=3,measure=mean){
  vectoraggr <- function(y,aggr){
    ord <- order(y)
    ngroup <- floor(length(y)/aggr)
    start <- 1+aggr*(0:(ngroup-1))
    end <- start+aggr-1
    end[length(end)] <- length(y)
    for(i in 1:length(start)){
      y[ord][start[i]:end[i]] <-measure(y[ord][start[i]:end[i]]) 
    }
    y
  }
  if(!is.null(by))
    if(any(!by%in%colnames(x)))
      stop(paste("Cannot find variable:",by[!by%in%colnames(x)]))
  if(any(!variables%in%colnames(x)))
    stop(paste("Cannot find variable:",variables[!variables%in%colnames(x)]))
  x$PRIMARYIDFORRESORTING <- 1:nrow(x)
  if(is.null(by)){
    by <- "BYVARIABLEFORSPLIT"
    x$BYVARIABLEFORSPLIT<-factor(1)
  }
  
  sp <- split(x,x[,by])
  for(i in 1:length(sp)){
    if(aggr>nrow(sp[[i]])){
      warning(paste("Only",nrow(sp[[i]]),"observations could be aggregated in group:",paste(apply(sp[[i]][,by,drop=FALSE],2,unique),collapse=" - ")))
    }
    sp[[i]][,variables] <- apply(sp[[i]][,variables,drop=FALSE],2,vectoraggr,aggr=aggr)
  }
  x <- unsplit(sp,x[,by])
  x <- x[order(x$PRIMARYIDFORRESORTING),]
  x <- x[,!colnames(x)%in%c("BYVARIABLEFORSPLIT","PRIMARYIDFORRESORTING")]
  return(x)
}