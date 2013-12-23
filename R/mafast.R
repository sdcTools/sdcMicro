setGeneric('mafast', function(obj, variables=NULL,by=NULL,aggr=3,measure=mean) {standardGeneric('mafast')})
setMethod(f='mafast', signature=c('sdcMicroObj'),
    definition=function(obj, variables=NULL,by=NULL,aggr=3,measure=mean) { 
      x <- get.sdcMicroObj(obj, type="manipNumVars")
      if(is.null(by)){
        if(!is.null(obj@strataVar))
          by <- colnames(obj@origData)[obj@strataVar]
      }else if(!all(by%in%colnames(x))){
        tmp <- obj@manipKeyVars[,by[by%in%colnames(obj@manipKeyVars)&!by%in%colnames(x)],drop=FALSE]
        x <- cbind(x,tmp)
        if(!all(by%in%colnames(x))){
          tmp <- obj@origData[,by[!by%in%colnames(x)],drop=FALSE]
          x <- cbind(x,tmp)
        }
      }
        
      if(is.null(variables)){
        variables <- colnames(obj@origData)[obj@numVars]
        variables <- variables[!variables%in%by]
      }
      res <- mafastWORK(x, variables=variables,by=by,aggr=aggr,measure=measure)
      obj <- nextSdcObj(obj)
      obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(as.data.frame(res[,colnames(get.sdcMicroObj(obj, type="manipNumVars"))])))
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
	ergy<-id <- NA#for CHECK-NOTES
    ngroup <- floor(length(y)/aggr)
    g <- rep(1:ngroup,each=aggr)
    nadd <- length(y)-length(g)
    if(nadd>0)
      g <- c(g,rep(g[length(g)],nadd))
    ord <- order(y)
    ord2 <- order(ord)
    yy <- data.table(y=y,g=g[ord2],id=1:length(y))
    setkey(yy,"g")
    erg <- merge(yy,yy[,j=list(ergy=measure(y)),by=g])
    setkey(erg,"id")
    return(erg[,ergy,by=id]$ergy)
  }
  BYVARIABLEFORSPLIT<-idvariableforresorting<-NA#for CHECK-NOTES
  if(!is.null(by))
    if(any(!by%in%colnames(x)))
      stop(paste("Cannot find variable:",by[!by%in%colnames(x)]))
  if(any(!variables%in%colnames(x)))
    stop(paste("Cannot find variable:",variables[!variables%in%colnames(x)]))
  if(is.null(by)){
    by <- "BYVARIABLEFORSPLIT"
    x$BYVARIABLEFORSPLIT<-factor(1)
  }else if(length(by)>1){
    x$BYVARIABLEFORSPLIT <- as.factor(apply(x,1,function(x)paste(x[by],collapse="-")))
    by <- "BYVARIABLEFORSPLIT"
  }else{
    x[,"BYVARIABLEFORSPLIT"] <- as.factor(x[,by])
    by <- "BYVARIABLEFORSPLIT"
  }
  x$idvariableforresorting <- 1:nrow(x)
  xdat <- data.table(x[,c(variables,by,"idvariableforresorting")])
  setkey(xdat,cols=BYVARIABLEFORSPLIT)
  idf <- function(x)x
  #TODO: Is there an better way to do this than to paste a cmd, it is fast though.
  cmd <- paste("erg <- xdat[,j=list(idvariableforresorting=idf(idvariableforresorting),",
      paste(variables,"= vectoraggr(",variables,",aggr=aggr)",collapse=",",sep=""),
      "),by=BYVARIABLEFORSPLIT]")
  erg <- vector() #To get no NOTE
  eval(parse(text=cmd))       
  x <- x[,!colnames(x)%in%c("BYVARIABLEFORSPLIT","idvariableforresorting"),drop=FALSE]
  setkey(erg,"idvariableforresorting")
  x[,variables] <- data.frame(erg[,by=idvariableforresorting])[,variables]
  return(x)
}