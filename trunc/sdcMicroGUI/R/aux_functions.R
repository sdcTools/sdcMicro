sdcGUIenv <- new.env()
## utility functions
# envionment with get and set functions
# not used, cause it ignores new env ... to use, remove ...x
#sdcGUIenvx <- function() {
#  pos <- match("sdcGUIenv", search())
#  if(is.na(pos)) {
#    sdcGUIenv <- list()
#    attach(sdcGUIenv, pos=length(search())-1)
#    rm(sdcGUIenv)
#    pos <- match("sdcGUIenv", search())
#  }
#  return(pos.to.env(pos))
#}

sdcGUIoutput <- function(){
  if(existd("sdcObject")){
    sdc <- ActiveSdcObject()
    return(extractManipData(sdc))
  }else
    stop("There is no object from the sdcGUI to retrieve.")
}
putd <- function(x, value) {
  assign(x, value, envir=sdcGUIenv) # add () to sdcGUIenv
}

rmd <- function(x) {
  rm(list=x, envir=sdcGUIenv) # rm () from sdcGUIenv
}


getd <- function(x, mode="any") {
  get(x, envir=sdcGUIenv, mode=mode, inherits=FALSE) # add () to sdcGUIenv
}

existd <- function(x, mode="any") {
  exists(x, envir=sdcGUIenv, mode=mode, inherits=FALSE) # add () to sdcGUIenv
}
listd <- function(x){
  ls(envir=sdcGUIenv)
}
ActiveDataSet <- function(name) {
  if( missing(name) ) {
    getd("activeDataSet")
  } else {
    if( is.matrix(get(name)) ) {
      putd("activeDataSet", data.frame(get(name), stringsAsFactors=FALSE))
    } else {
      putd("activeDataSet", get(name))
    }
    putd("dataSetName", name)
  }
}
ActiveSdcObject <- function(name) {
  if( missing(name) ) {
    getd("sdcObject")
  } else {
    if( class(name)=="sdcMicroObj" ) {
      putd("sdcObject", name)
    } else {
      stop("Input is not an object of class 'sdcMicroObj'")
    }
  }
}
ActiveSdcVars <- function(name="keyVars"){
  get.sdcMicroObj(getd("sdcObject"),name)
}
ActiveSdcVarsStr <- function(name="keyVars"){
  sdcObject <- getd("sdcObject")
  colnames(sdcObject@origData)[get.sdcMicroObj(sdcObject,name)]
}
parseVar <- function(x, ...) {
  if(length(x)==0)return("NULL")
  s <- "c("
  for ( i in 1:length(x) ) {
    s <- paste(s, x[i])
    if (i < length(x)) {
      s <- paste(s, ",")
    }
  }
  s <- paste(s, ")")
  return(s)
}

parseVarStr <- function(x, ...) {
  if(length(x)==0)return("NULL")
  s <- "c("
  for ( i in 1:length(x) ) {
    s <- paste(s, "'", x[i], "'", sep="")
    if (i < length(x)) {
      s <- paste(s, ",", sep="")
    }
  }
  s <- paste(s, ")", sep="")
  return(s)
}

# getIndex to get the col index of categorical, numerical and weight vars
getIndex <- function(x, ...) {
  ads <- names(ActiveDataSet())
  ord <- c()
  for( i in 1:length(x) ) {
    for( j in 1:length(ads) ) {
      if( x[i]==ads[j] ) {
        ord <- c(ord, j)
      }
    }
  }
  return(ord)
}
printFrequencies <- function(obj){
  cat("\n --------------------------\n")
  cat(paste(sum(obj@risk$individual[,2]<2), "obs. violate 2-anonymity \n"))
  cat(paste(sum(obj@risk$individual[,2]<3), "obs. violate 3-anonymity \n"))
  cat(" --------------------------\n")  
}
printFrequenciesComp <- function(obj){
#  cat("\n --------------------------\n")
  cat("Number of observations violating\n")
  cat("\n -  2-anonymity:  ")
  cat(paste(sum(obj@risk$individual[,2]<2),
          "(orig: ",sum(obj@originalRisk$individual[,2]<2),")\n"))
  cat(" -  3-anonymity:  ")
  cat(paste(sum(obj@risk$individual[,2]<3),
          "(orig: ",sum(obj@originalRisk$individual[,2]<3),")"))
#  cat(paste(sum(obj@originalRisk$individual[,2]<2), "obs. violate 2-anonymity \n"))
#  cat(paste(sum(obj@originalRisk$individual[,2]<3), "obs. violate 3-anonymity \n"))
  cat("\n--------------------------\n")  
  n <- nrow(obj@origData)
  cat("\nPercentage of observations violating\n")
  cat(" -  2-anonymity:  ")
  cat(paste(round(sum(obj@risk$individual[,2]<2)/n*100,2),"% ",
          "(orig: ",round(sum(obj@originalRisk$individual[,2]<2)/n*100,2),"%",")\n"))
  cat(" -  3-anonymity:  ")
  cat(paste(round(sum(obj@risk$individual[,2]<3)/n*100,2),"% ",
          "(orig: ",round(sum(obj@originalRisk$individual[,2]<3)/n*100,2),"%",")"))
}
printMeasure_risk <- function(obj){
  risk <- obj@risk
  cat("\n")
  cat("--------------------------\n")
  s <- sum((risk$individual[,1] > median(risk$individual[,1])+2*mad(risk$individual[,1])) & (risk$indiviual[,1] > 0.1))
  cat(paste(s,"obs. with higher risk than the main part\n"))
  cat("Expected no. of re-identifications:\n",round(risk$global$risk_ER,2),"")
  cat("(",round(risk$global$risk_pct,2),"%)\n")
  if(is.na(risk$global$threshold))
    risk$global$threshold <- Inf
  #cat("Threshold:",round(risk$global$threshold,2),"\n (for maximal global risk",round(risk$global$max_risk,2),")\n")
  cat("--------------------------\n")
  if("hier_risk_ER"%in%names(risk$global)){
    if(!is.na(risk$global$hier_risk_ER)){
      cat("--------------------------\n")
      cat("Hierarchical risk \n")
      cat("--------------------------\n")
      cat("Expected no. of re-identifications:\n",round(risk$global$hier_risk_ER,2),"")
      cat("(",round(risk$global$hier_risk_pct,2),"% )\n")
    }else{
      cat("--------------------------\n")
      cat("Hierarchical risk not available\n")
      cat("--------------------------\n")
    }
  }
}
printRecode <- function(obj){
  cat("Reported is the")
  cat("\n")
  cat(" number | mean size and | size of smallest category")
  cat("\n")
  k <- length(obj@keyVars)
  tab <- tab2 <- ssize <- ssize2 <- msize <- msize2 <- numeric(k)
  names(tab) <- colnames(obj@origData[,obj@keyVars])
  cat("\n")
  for(i in 1:k){
    tab2[i] <- length(unique(obj@origData[,obj@keyVars[i]]))
    tab[i] <- length(unique(obj@manipKeyVars[,i]))
    t2 <- table(obj@origData[,obj@keyVars[i]])
    t1 <- table(obj@manipKeyVars[,i])
    msize[i] <- round(mean(t1),0)
    msize2[i] <- round(mean(t2),0)
    ssize[i] <- min(t1)
    ssize2[i] <- min(t2)
  }
  nc <- sapply(names(tab), nchar)
  maxnam <- max(nc)
  for(i in 1:k){
#	  cat(names(tab)[i],":",tab[i]," (orig:", tab2[i],"), ms:", msize[i], "(orig:",msize2[i],") \n")
    nam <- names(tab)[i]
    cat("-------------\n")
    cat(nam, paste(rep(".",2+maxnam-nchar(nam)), collapse=""),tab[i],"|",msize[i],"|",ssize[i], 
        "\n     (orig:", tab2[i],"|",msize2[i],"|",ssize2[i],") \n")
  }
}
printMeasure_riskComp <- function(obj){
#  cat("NOW:")
  risk <- obj@risk
  originalRisk <- obj@originalRisk
  cat("\n")
  cat("--------------------------\n")
  s <- sum((risk$individual[,1] > median(risk$individual[,1])+2*mad(risk$individual[,1])) & (risk$indiviual[,1] > 0.1))
  sorig <- sum((originalRisk$individual[,1] > median(originalRisk$individual[,1])+2*mad(originalRisk$individual[,1])) & (originalRisk$indiviual[,1] > 0.1))
  cat(paste(s," (orig:", sorig, ")","obs. with higher risk than the main part\n"))
  cat("Expected no. of re-identifications:\n",round(risk$global$risk_ER,2),"")
  cat("[",round(risk$global$risk_pct,2),"%]  (orig:", round(originalRisk$global$risk_ER,2), 
      "[",round(originalRisk$global$risk_pct,2),"%])\n")
#  if(is.na(risk$global$threshold))
#    risk$global$threshold <- Inf
  #cat("Threshold:",round(risk$global$threshold,2),"\n (for maximal global risk",round(risk$global$max_risk,2),")\n")
  cat("--------------------------\n")
  if("hier_risk_ER"%in%names(risk$global)){
    if(!is.na(risk$global$hier_risk_ER)){
      cat("--------------------------\n")
      cat("Hierarchical risk \n")
      cat("--------------------------\n")
      cat("Expected no. of re-identifications:\n",
          round(risk$global$hier_risk_ER,2),"")
      cat("[",round(risk$global$hier_risk_pct,2),"%]  (orig:", 
          round(originalRisk$global$hier_risk_ER,2), 
          "[",round(originalRisk$global$hier_risk_pct,2),"%])\n")
    }else{
      cat("--------------------------\n")
      cat("Hierarchical risk not available\n")
      cat("--------------------------\n")
    }
  }
#  cat("ORIGINAL:")
#  risk <- obj@originalRisk
#  cat("\n")
#  cat("--------------------------\n")
#  s <- sum((risk$individual[,1] > median(risk$individual[,1])+2*mad(risk$individual[,1])) & (risk$indiviual[,1] > 0.1))
#  cat(paste(s,"obs. with higher risk than the main part\n"))
#  cat("Expected no. of re-identifications:\n",round(risk$global$risk_ER,2),"")
#  cat("(",round(risk$global$risk_pct,2),"%)\n")
#  if(is.na(risk$global$threshold))
#    risk$global$threshold <- Inf
#  #cat("Threshold:",round(risk$global$threshold,2),"\n (for maximal global risk",round(risk$global$max_risk,2),")\n")
#  cat("--------------------------\n")
#  if("hier_risk_ER"%in%names(risk$global)){
#    if(!is.na(risk$global$hier_risk_ER)){
#      cat("--------------------------\n")
#      cat("Hierarchical risk \n")
#      cat("--------------------------\n")
#      cat("Expected no. of re-identifications:\n",round(risk$global$hier_risk_ER,2),"")
#      cat("(",round(risk$global$hier_risk_pct,2),"% )\n")
#    }else{
#      cat("--------------------------\n")
#      cat("Hierarchical risk not available\n")
#      cat("--------------------------\n")
#    }
#  }
  
}

printLocalSuppression <- function(obj){
  keyVars <- colnames(obj@manipKeyVars)
  maxnam <- max(sapply(keyVars, nchar))
  if(is.null(obj@localSuppression))
    lsup <- list(rep(0,length(keyVars)))
  else
    lsup <- obj@localSuppression
  for(i in 1:length(keyVars)){
    nam <- keyVars[i]
    n <- nrow(obj@origData)
    cat("\n")
    cat(keyVars[i],paste(rep(".",2+maxnam-nchar(nam)), collapse=""),lsup[[1]][i])
    cat(" [", round(100*lsup[[1]][i]/n,3), "%]")
  }
}


updates2 <- function(restart=FALSE){
  options(timeout=5)
  xt <- try(download.file(url="http://cran.r-project.org/",destfile=tempfile(),quiet=TRUE))
  INET <- TRUE
  if(class(xt)=="try-error"){
    INET <- FALSE
  }
  if(INET){
    oldP <- old.packages()
    if(!is.null(oldP)){
      oldP <- oldP[oldP[,1]%in%c("sdcMicro","sdcMicroGUI"),,drop=FALSE]  
    }else{
      oldP <- data.frame()
    }
    
    if(nrow(oldP)!=0){
      text <- paste("Updates found for the following packages: ",paste(oldP[,1],collapse="\n ",sep=""),"\n Click OK for updating (GUI will be restarted).",sep="")
    }else{
      text <- "No updates available."
    }
    if(text=="No updates available."&&!restart){
      return(0)
    }
    ns_do <- gconfirm(text, title="Package Updates",icon="warning")
    if( ns_do &&substr(text,1,1)!="N") {
      loaded <- oldP[oldP[,1]%in%loadedNamespaces(),1]
      for(ll in loaded){
        if(length(which(search()==paste("package:",ll,sep="")))>0)
          detach(pos=which(search()==paste("package:",ll,sep="")),unload=TRUE,force=TRUE)
      }
      update.packages(oldPkgs=oldP,ask=FALSE)
      for(ll in loaded)
        require(ll,character.only=TRUE)
      
      if(restart)
        sdcGUI()
    }
  }else
    gmessage("It is not possible to check for possible updates at the moment.", title="No internet connection",icon="warning")
}
