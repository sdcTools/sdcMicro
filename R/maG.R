sampleCat <- function(x){
  #sample with probabilites corresponding to there number in the NNs
  if(!is.factor(x))
    x <- as.factor(x)
  s <- summary(x)
  s <- s[s!=0]
  sample(names(s),1,prob=s)
}
maxCat <- function(x){
  #choose cat with max prob, random if max is not unique
  if(!is.factor(x))
    x <- as.factor(x)
  s <- summary(x)
  s <- s[s!=0]
  if(sum(s>0)>1)
    s <- sample(s)
  names(s)[which.max(s)]
}
which.minN <- function(x,n){
  n <- min(n,length(x))
  out <- vector()
  for(i in 1:n){
    out[i] <-which.min(x)
    x[which.min(x)] <- Inf
  }
  as.numeric(out)
}
## Wrapper function for gowerD
gowerD <- function(data.x, data.y = data.x, weights=NULL,numerical,factors,orders,mixed,levOrders,mixed.constant) {
  maxplus1 <- function(x){
    x[is.na(x)] <- max(x,na.rm=TRUE)+1
    x
  }
  #weights <- rep(1,ncol(data.x))
  for(i in 1:ncol(data.x)){
    data.x[,i] <- as.numeric(data.x[,i])
    data.y[,i] <- as.numeric(data.y[,i])
  }
  weightind <- order(match(colnames(data.x),c(numerical,factors,orders,mixed)))
  data.x <- data.x[,c(numerical,factors,orders,mixed),drop=FALSE]
  data.y <- data.y[,c(numerical,factors,orders,mixed),drop=FALSE]
  
  justone <- FALSE
  if(nrow(data.y)==1){
    data.y <- rbind(data.y,data.y)
    justone <- TRUE
  }
  data.x <- apply(data.x,2,maxplus1)
  data.y <- apply(data.y,2,maxplus1)
  levOrders <- as.numeric(levOrders)
  
  out <- .Call( "gowerD", data.x, data.y,weights[weightind],c(length(numerical),
          length(factors),length(orders),length(mixed)),levOrders,mixed.constant,PACKAGE="sdcMicro")
  if(justone)
    out <-  out$delta[,1,drop=FALSE]
  else
    out <- out$delta
  out
}

##Fuer data.frame, kNN-Suche auf Basis der Gowerdistance und Aggregation mit numFun bzw. catFun
maGowerWORK <- function(data,variables=colnames(data),aggr=3,dist_var=variables,by=NULL,mixed=NULL,mixed.constant=NULL,
    trace=FALSE,weights=NULL,
    numFun = mean, catFun=sampleCat,
    addRandom=FALSE){
  if(!is.null(by)){
    if(!all(by%in%colnames(data)))
      stop("'by'-variable not found in data set.\n'by' should be a vector of
              variable names to split the dataset before \n applying microaggregation.")
    splfac <- as.factor(apply(data[,by,drop=FALSE],1,function(x)paste(as.character(x),collapse="--")))
    spl <- split(data,splfac)
  }else{
    splfac <- as.factor(rep(1,nrow(data)))
    spl <- list(data)
  }
  orders <- vector()
  for(i in 1:ncol(data)){
    orders <- c(orders,is.ordered(data[,i]))
  }
  orders <- colnames(data)[orders]
  levOrders <- vector()
  if(length(orders)>0){
    for(i in 1:length(orders)){
      levOrders[i] <- levels(data[,orders[i]])[length(levels(data[,orders[i]]))]
    }
  }
  factors <- vector()
  for(i in 1:ncol(data)){
    factors <- c(factors,is.factor(data[,i]))
  }
  factors <- colnames(data)[factors]
  factors <- factors[!factors%in%orders]
  numerical <- vector()
  for(i in 1:ncol(data)){
    numerical <- c(numerical,is.numeric(data[,i])|is.integer(data[,i]))
  }
  numerical <- colnames(data)[numerical]
  numerical <- numerical[!numerical%in%mixed]
  if(trace){
    cat("Detected as categorical variable:\n")
    print(factors)
    cat("Detected as ordinal variable:\n")
    print(orders)
    cat("Detected as numerical variable:\n")
    print(numerical)  
  }
  if(is.null(weights)){
    weights <- rep(1,length(dist_var))
  }else if(length(weights)!=length(dist_var)){
    stop("length of weights must be equal the number of distance variables")
  }
  if(addRandom){
    numerical <- c(numerical, "RandomVariableForImputation")
    data[,"RandomVariableForImputation"] <- rnorm(nrow(data))
    if(is.list(dist_var)){
      for(i in 1:length(dist_var)){
        dist_var[[i]] <- c(dist_var[[i]],"RandomVariableForImputation")
        weights[[i]] <- c(weights[[i]],min(weights[[i]])/(sum(weights[[i]])+1))
      }
    }else{
      dist_var <- c(dist_var,"RandomVariableForImputation")
      weights <- c(weights,min(weights)/(sum(weights)+1))
    }
  }
  numericalX <-numerical[numerical%in%dist_var]
  factorsX <-factors[factors%in%dist_var]
  ordersX <-orders[orders%in%dist_var]
  levOrdersX <- levOrders[orders%in%dist_var]
  #print(levOrdersX)
  mixedX <-mixed[mixed%in%dist_var]
  if(is.null(mixed.constant))
    mixed.constant <- rep(0,length(mixedX))
  spl <- lapply(spl,function(dataSpl){
        gd <- gowerD(dataSpl[,dist_var],dataSpl[,dist_var],weights=weights,numericalX,factorsX,ordersX,mixedX,levOrdersX,mixed.constant=mixed.constant);
        which.minNk <- function(x)1
        cmd <- paste("which.minNk <- function(x)which.minN(x,",aggr,")",sep="")
        eval(parse(text=cmd))
        mindi <- apply(gd,2,which.minNk)
        erg <- as.matrix(mindi)
        if(addRandom)
          dataSpl <- dataSpl[,-which(colnames(dataSpl)=="RandomVariableForImputation")]
        dataSpl[,variables] <- do.call("rbind",apply(erg,2,function(x){
                  as.data.frame(lapply(dataSpl[x,variables],function(x){
                            if(is.factor(x))
                              return(catFun(x))
                            else
                              return(numFun(x))
                          }))
                }))
        return(dataSpl)
      })
  rn <- row.names(data)
  data <- do.call("rbind",spl)
  row.names(data) <- rn
  #data <-cbind(data,t(erg))
  return(data)
}

setGeneric('microaggrGower', function(obj, variables=NULL,aggr=3,dist_var=NULL,by=NULL,
        mixed=NULL,mixed.constant=NULL,
        trace=FALSE,weights=NULL,
        numFun = mean, catFun=sampleCat,
        addRandom=FALSE) {standardGeneric('microaggrGower')})
setMethod(f='microaggrGower', signature=c('sdcMicroObj'),
    definition=function(obj, variables=NULL,aggr=3,dist_var=NULL,by=NULL,
        mixed=NULL,mixed.constant=NULL,
        trace=FALSE,weights=NULL,
        numFun = mean, catFun=sampleCat,
        addRandom=FALSE) {
      o <- extractManipData(obj)
      if(is.null(by)&&"sdcGUI_strataVar"%in%colnames(o))
        by <- "sdcGUI_strataVar"
      nV <- get.sdcMicroObj(obj, type="numVars")
      kV <- get.sdcMicroObj(obj, type="keyVars")
      if(is.null(variables))
        variables <- colnames(o)[c(nV,kV)]
      if(is.null(dist_var))
        dist_var <- variables
      res <- maGowerWORK(o,
          variables=variables,aggr=aggr,dist_var=dist_var,by=by,
          mixed=mixed,mixed.constant=mixed.constant,
          trace=trace,weights=weights,
          numFun = numFun, catFun=catFun,
          addRandom=addRandom)
      
      obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(as.data.frame(res[,nV])))
      obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(as.data.frame(res[,kV])))
      obj <- dRisk(obj)
      obj <- dUtility(obj)
      obj <- calcRisks(obj)
      obj
    })
setMethod(f='microaggrGower', signature=c("data.frame"),
    definition=function(obj,
        variables=colnames(data),
        aggr=3,
        dist_var=variables,
        by=NULL,
        mixed=NULL,
        mixed.constant=NULL,
        trace=FALSE,weights=NULL,
        numFun = mean, catFun=sampleCat,
        addRandom=FALSE) { 
      maGowerWORK(data=obj,variables=variables,dist_var=dist_var,by=by,mixed=mixed,
          mixed.constant=mixed.constant,trace=trace,weights=weights,numFun=numFun,catFun=catFun,addRandom=addRandom)
    })
#data(testdata,package="sdcMicro)
#for(i in c(1:7,9))testdata[,i] <- as.factor(testdata[,i])

#test <- maG(testdata,variables=C("relat","age","expend"),dist_var=C("age","sex","income","savings"),BY=C("urbrur","roof"))

