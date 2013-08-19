setGeneric('LLmodGlobalRisk', function(obj, method = "IPF", inclProb = NULL, form = NULL, modOutput = FALSE) {standardGeneric('LLmodGlobalRisk')})
setMethod(f='LLmodGlobalRisk', signature=c('sdcMicroObj'),
    definition=function(obj, method = "IPF", inclProb = NULL, form = NULL, modOutput = FALSE) { 
      if(is.null(form)){
        x <- get.sdcMicroObj(obj, type="manipKeyVars")
        form <- as.formula(paste(" ~ ", paste(colnames(x), collapse= "+")))
      }else{ 
        vars <- labels(terms(form))
        mk <- get.sdcMicroObj(obj, type="manipKeyVars")
        mn <- get.sdcMicroObj(obj, type="manipNumVars")
        ok <- get.sdcMicroObj(obj, type="origData")
        ok <- ok[,!colnames(ok)%in%c(colnames(mk),colnames(mn)),drop=FALSE]
        if(any(colnames(mk)%in%vars)){
          x <- mk[,colnames(mk)%in%vars,drop=FALSE]
        }else
          x <- NULL
        if(any(colnames(mn)%in%vars)){
          if(is.null(x))
            x <- mn[,colnames(mn)%in%vars,drop=FALSE]
          else 
            x <- data.frame(x,mn[,colnames(mn)%in%vars,drop=FALSE])
        }
        if(any(colnames(ok)%in%vars)){
          if(is.null(x))
            x <- ok[,colnames(ok)%in%vars,drop=FALSE]
          else 
            x <- data.frame(x,ok[,colnames(ok)%in%vars,drop=FALSE])
        }
      }
      if(is.null(inclProb)&&!is.null(get.sdcMicroObj(obj,type="weightVar"))){
        inclProb <- 1/get.sdcMicroObj(obj,type="origData")[,get.sdcMicroObj(obj,type="weightVar")]
      }
      
      risk <- get.sdcMicroObj(obj, type="risk")
      risk$model <- LLmodGlobalRiskWORK(x=x,method=method,inclProb=inclProb,form=form,modOutput=modOutput)
      risk$model$inclProb <- inclProb
      obj <- set.sdcMicroObj(obj, type="risk", input=list(risk))
      obj
    })
setMethod(f='LLmodGlobalRisk', signature=c("data.frame"),
    definition=function(obj, method = "IPF", inclProb = NULL, form = NULL, modOutput = FALSE) { 
      if(is.null(form))
        form <- as.formula(paste(" ~ ", paste(colnames(obj), collapse= "+")))
      LLmodGlobalRiskWORK(x=obj,method=method,inclProb=inclProb,form=form,modOutput=modOutput)
    })
setMethod(f='LLmodGlobalRisk', signature=c("matrix"),
    definition=function(obj, method = "IPF", inclProb = NULL, form = NULL, modOutput = FALSE) {
      if(is.null(form))
        form <- as.formula(paste(" ~ ", paste(colnames(obj), collapse= "+")))
      LLmodGlobalRiskWORK(x=obj,method=method,inclProb=inclProb,form=form,modOutput=modOutput)
    })


LLmodGlobalRiskWORK <- function(x, method="IPF", inclProb=NULL,  
    form=as.formula(paste(" ~ ", paste(colnames(x), collapse= "+"))), 
    modOutput=FALSE){
  if(is.null(inclProb)){
    warning("please provide the inclusion probabilities, \n e.g. 
            approx by 1/sampling weights. \n
            They are now set to 0.1 which is simple a wrong assumption.")
    inclProb = 0.1
  }
  #x
  #risk functions
  #P(F_k=r | f_k = r)
  risk1 <-  function(l,p) {
    v = (1-p)*l
    exp(-v)
  }
  #E(1/F_k | f_k = 1)
  risk2 <- function(l,p) {
    v = (1-p)*l
    (1-exp(-v))/v
  }	
  #file level risk measure
  file_risk <- function(freq,risk) {
    sum(as.numeric(freq==1) * risk)
  }
  
  ## sample frequencies
  tab <- xtabs(form, x)
  x <- data.frame(x,inclProb=inclProb)
  form2 <- as.formula(paste(c("inclProb",as.character(form)),collapse=""))
  tabP <-  xtabs(form2, x)
  
  ## IPF
  mod <- loglm(form, data=tab, fitted=TRUE)
  lambda <- fitted(mod)

  ## Risk
  r1 <- risk1(lambda, tabP)
  r2 <- risk2(lambda, tabP)
  gr1 <- file_risk(tab, r1)/nrow(x)
  gr2 <- file_risk(tab, r2)/nrow(x)
  if(modOutput) {
    res <- list(gr1=gr1, gr2=gr2, gr1perc=gr1*100, gr2perc=gr2*100, tab=tab, fitted=lambda)		
  } else{ 
    res <- list(gr1=gr1, gr2=gr2, gr1perc=gr1*100, gr2perc=gr2*100)
  }
  res
} 

#data(free1)
#x <- data.frame(free1[,c(2,4:5)])
#x["SEX"] <- as.factor(x[,"SEX"])
#x["MARSTAT"] <- as.factor(x[,"MARSTAT"])
#x["KINDPERS"] <- as.factor(x[,"KINDPERS"])
#LLmodGlobalRisk(x, inclProb=1/mean(free1[,"WEIGHT"]))




## make a contingency table
#data(free1)
#x <- free1[,1:4]
#tab <- table(as.data.frame(x))
#tab
#
##risk functions
##P(F_k=r | f_k = r)
#risk1 = function(l,p) {v = (1-p)*l; exp(-v)}
##E(1/F_k | f_k = 1)
#risk2 = function(l,p) {v = (1-p)*l; (1-exp(-v))/v}
#
##file level risk measure
#file_risk = function(freq,risk) {sum(as.numeric(freq==1) * risk)}
#
##selection probability
#prob_sel=0.1


######################################



## resource from data privacy work...
##artificial contingency table
#x1 = as.factor(c("A","B","C","A","B","C"))
#x2 = as.factor(c(0,0,0,1,1,1))
#levels(x2)=c("m","f")
#y = c(1,5,3,4,3,2)
#
#show_as.table = function(vec) {#
#	matrix(vec,nrow=2,ncol=3,byrow=TRUE,dimnames=list(levels(x2),levels(x1))) }
#show_as.table(y)
#
##risk functions
##P(F_k=r | f_k = r)
#risk1 = function(l,p) {v = (1-p)*l; exp(-v)}
##E(1/F_k | f_k = 1)
#risk2 = function(l,p) {v = (1-p)*l; (1-exp(-v))/v}
#
##file level risk measure
#file_risk = function(freq,risk) {sum(as.numeric(freq==1) * risk)}
#
##selection probability
#prob_sel=0.1
#
##log-lin model
##y...sample frequencies
##no inclusion of interaction terms e.g. (x1+x2)2
#form = y~x1+x2 
#mod = glm(form,family="poisson")
#
#predict(mod)
#
#
#
##predicted population frequencies(?)
##what is predicted here? 
##lambda or population frequencies? or is this the same, as E(Po(lambda)) = lambda
#lambda = exp(predict(mod))
#matrix(lambda,nrow=2,ncol=3,byrow=TRUE,dimnames=list(levels(x2),levels(x1)))#
#
##calculate risk measure
#show_as.table(y)
#show_as.table(risk1(lambda,prob_sel))
#file_risk(y,risk1(lambda,prob_sel))
#
#show_as.table(y)
#show_as.table(risk2(lambda,prob_sel))
#file_risk(y,risk2(lambda,prob_sel))
#
###########################
##IPF implementation
##not sure this example makes a lot of sense, 
##since it is only a few data and possibly no uniques
#
#library ( sdcMicro )
#data ( francdat )
#
#set.seed(123)
#lines = sample(1:length(francdat),40,replace=TRUE)
#x <- francdat [lines,c(2 ,4 ,5 ,6)]; 
#m = xtabs(~.,data=x)
#mod = loglm(~Key1+Key2+Key3+Key4,data=m)
#lambda = fitted(mod)
#
#prob_sel=0.1
#risk1(lambda,prob_sel)
#
## IPF



#data(free1)
#x <- data.frame(free1[,c(2,4:5)])
#x["SEX"] <- as.factor(x[,"SEX"])
#x["MARSTAT"] <- as.factor(x[,"MARSTAT"])
#x["KINDPERS"] <- as.factor(x[,"KINDPERS"])
#LLmodGlobalRisk(x)



