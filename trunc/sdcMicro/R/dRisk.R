setGeneric('dRisk', function(obj, ...) {standardGeneric('dRisk')})
setMethod(f='dRisk', signature=c('sdcMicroObj'),
    definition=function(obj, ...) { 
      numVars <- get.sdcMicroObj(obj, type="numVars")
      x <- get.sdcMicroObj(obj, type="origData")[,numVars,drop=F]
      xm <- get.sdcMicroObj(obj, type="manipNumVars")
      risk <- get.sdcMicroObj(obj, type="risk")
      risk$numeric <- dRiskWORK(x=x, xm=xm, ...)
      obj <- set.sdcMicroObj(obj, type="risk", input=list(risk))
      
      obj
    })
setMethod(f='dRisk', signature=c("data.frame"),
    definition=function(obj, ...) { 
      dRiskWORK(x=obj,...)
    })
setMethod(f='dRisk', signature=c("matrix"),
    definition=function(obj, ...) { 
      dRiskWORK(x=obj,...)
    })
dRiskWORK <- function (x, xm, k = 0.05)
{
  if (dim(x)[1] != dim(xm)[1]) {
    warning("dimension of perturbed data and original data are different")
    xm <- xm[1:dim(x)[1], ]
  }
  sds <- apply(xm, 2, sd, na.rm=TRUE)
  mi <- t(t(xm) - k * sds)
  ma <- t(t(xm) + k * sds)
  w <- which(rowSums(x < mi | x > ma, na.rm=TRUE) %in% 0:1)
  as.numeric(length(w)/nrow(x))
}