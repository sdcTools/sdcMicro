setGeneric('LocalRecProg', function(obj, ancestors=NULL,ancestor_setting=NULL,
        k_level=2, FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA,...) {standardGeneric('LocalRecProg')})
setMethod(f='LocalRecProg', signature=c('sdcMicroObj'),
    definition=function(obj,ancestors=NULL,ancestor_setting=NULL,
        k_level=2, FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA, ...) { 
      keyVars <- get.sdcMicroObj(obj, type="manipKeyVars")
      manipData <- keyVars
      keyVarIndices <- colnames(manipData)
      numVars <- get.sdcMicroObj(obj, type="manipNumVars")
      if(!is.null(numVars)) {
        manipData <- cbind(manipData, numVars)
        numVarIndices <- colnames(numVars)
      } else numVarIndices <- NULL
      kVlr <- paste(colnames(manipData),"_lr",sep="")
      kV <- colnames(manipData)
      if(!is.null(ancestors)){
        if(!all(ancestors%in%colnames(manipData))){
          origData <- get.sdcMicroObj(obj, type="origData")
          manipData <- cbind(manipData,origData[,ancestors[!ancestors%in%colnames(manipData)]])
        }
      }   
      res <- LocalRecProgWORK(manipData,keyVarIndices,numerical=numVarIndices,ancestors=ancestors,ancestor_setting=ancestor_setting,
          k_level=k_level, FindLowestK=FindLowestK, weight=weight, lowMemory=lowMemory, missingValue=missingValue)
      
      newData <- res[,kVlr]
      colnames(newData) <- kV
      obj <- nextSdcObj(obj)
      obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(newData[,keyVarIndices]))
      if(!is.null(numVarIndices)) {
        obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(newData[,numVarIndices]))
      }
      obj <- calcRisks(obj)
      
      obj
    })
setMethod(f='LocalRecProg', signature=c("data.frame"),
    definition=function(obj,ancestors=NULL,ancestor_setting=NULL,
        k_level=2, FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA,categorical,numerical=NULL) { 
      LocalRecProgWORK(data=obj,categorical=categorical,numerical=numerical,ancestors=ancestors,ancestor_setting=ancestor_setting,
          k_level=k_level, FindLowestK=FindLowestK, weight=weight, lowMemory=lowMemory, missingValue=missingValue)
    })
setMethod(f='LocalRecProg', signature=c("matrix"),
    definition=function(obj,ancestors=NULL,ancestor_setting=NULL,
        k_level=2, FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA,categorical,numerical=NULL) { 
      LocalRecProgWORK(data=obj,categorical=categorical,numerical=numerical,ancestors=ancestors,ancestor_setting=ancestor_setting,
          k_level=k_level, FindLowestK=FindLowestK, weight=weight, lowMemory=lowMemory, missingValue=missingValue)
    })



LocalRecProgWORK <- function(data,categorical,numerical=NULL,ancestors=NULL,ancestor_setting=NULL,
    k_level=2, FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA)
{
  range=TRUE
  if(!is.null(ancestors)&&!is.null(ancestor_setting)){
    s1 <- which(categorical%in%ancestor_setting)-1
    s2 <- c()
    for(ca in categorical[which(categorical%in%ancestor_setting)]){
      s2 <- c(s2,sum(ca==ancestor_setting))
    }
    ancestor_settings <- cbind(s1,s2)
  }else if(!is.null(ancestors)&&is.null(ancestor_setting))
    stop("Please specify ancestor_setting if you want to use ancestors!\n")
  
  if(is.null(ancestors))
    ancestor_settings <- matrix()
  if(is.null(weight))
    weight <- rep(1,length(categorical)+length(numerical))
  if(length(weight)!=(length(categorical)+length(numerical)))
    stop("Length of weights must equal number of categorical variables plus number of numerical variables!\n")
  weight <- cbind(weight, c(rep(1,length(categorical)),rep(0,length(numerical))))
  
  
  dataX <- as.matrix(data[,c(categorical,numerical, ancestors), drop=FALSE]) 
  
  res <- .Call("LocalRecProg", dataX, k_level, FindLowestK, ancestor_settings, weight, range, FALSE, lowMemory, missingValue)
  colnames(res$Res) <- paste(c(categorical,numerical),"_lr",sep="")
  cbind(data[,c(categorical,numerical)],res$Res)
}
#require(sdcMicro)
## Link zum Testdatenset (data(testdata) enthaelt die ancestors nicht)
#setwd("M:\\Kowarik\\WorkspaceEclipse\\sdcMicro\\IHSN-SDC-Cpp\\IHSN-SDC\\Yichun 2 February 2009\\anonymization\\test\\win")
#dataset <- read.table("Test7_data.txt",sep="\t",header=TRUE)
#
#variables <- c("urbrur", "roof", "walls", "water", "sex", "relat")
#ancestors <- NULL#c("water2", "water3", "relat2")
#ancestor_setting <- cbind(c(3,5), c(2,1))
#ancestor_setting <- c("water","water","relat")
#weight <- cbind(rep(1,length(variables)), rep(TRUE,length(variables)))
#
#dataX <- as.matrix(dataset[,c(variables, ancestors), drop=FALSE]) 
#k_level <- 2
#FindLowestK <- TRUE
#range <- FALSE
#categoryCount <- FALSE
#lowMemory <- FALSE
#
#res <- .Call("LocalRecProg", dataX, k_level, FindLowestK, ancestor_setting, weight, range, categoryCount, lowMemory, NA)
#
#out <- read.table("out_test7.txt",sep="\t",header=TRUE,na=".")[,20:25]
#xx=cbind(out,rep("X",nrow(out)),res$Res)
#xx[xx==999999] <- 99
#xx
#
#out[is.na(out)] <- 998
#res$Res[is.na(res$Res)] <- 998
#TF <- out==res$Res
#tf=(!apply(TF,1,all))
#sum(tf)
#xx[tf,]
#
#r1=LocalRecProg(dataset,categorical=c("urbrur", "roof", "walls", "water", "sex", "relat"),missingValue=-99)
#r2=LocalRecProg(dataset,categorical=c("urbrur", "roof", "walls", "water", "sex", "relat"),
#    ancestor=c("water2", "water3", "relat2"),ancestor_setting=c("water","water","relat"),missingValue=-99)
