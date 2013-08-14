setGeneric('dUtility', function(obj, ...) {standardGeneric('dUtility')})
setMethod(f='dUtility', signature=c('sdcMicroObj'),
    definition=function(obj, ...) { 
      numVars <- get.sdcMicroObj(obj, type="numVars")
      x <- get.sdcMicroObj(obj, type="origData")[,numVars,drop=F]
      xm <- get.sdcMicroObj(obj, type="manipNumVars")
      utility <- get.sdcMicroObj(obj, type="utility")
      utility$il1 <- dUtilityWORK(x=x, xm=xm, method="IL1",...)
      utility$eigen <- dUtilityWORK(x=x, xm=xm, method="eigen",...)
      #utility$robeigen <- dUtilityWORK(x=x, xm=xm, method="robeigen",...)
      obj <- set.sdcMicroObj(obj, type="utility", input=list(utility))
      obj
    })
setMethod(f='dUtility', signature=c("data.frame"),
    definition=function(obj, ...) { 
      dUtilityWORK(x=obj,...)
    })
setMethod(f='dUtility', signature=c("matrix"),
    definition=function(obj, ...) { 
      dUtilityWORK(x=obj,...)
    })
dUtilityWORK <- function (x, xm, method = "IL1")
{
  if (dim(x)[1] != dim(xm)[1]) {
    warning("dimension of perturbed data and original data are different")
    xm <- xm[1:dim(x)[1], ]
  }
  if (method == "IL1") {
    a <- x
    for (i in 1:dim(x)[2]) {
      a[, i] <- abs((x[, i] - xm[, i])/sd(x[, i], na.rm=TRUE) * sqrt(2))
    }
    infLoss1 <- 1/(dim(x)[2]*dim(x)[1]) * sum(a, na.rm=TRUE)
    return(infLoss1)
  }
  if (method == "eigen") {
    e1 <- eigen(var(scale(x), na.rm=TRUE,use="pairwise.complete.obs"))$values
    e2 <- eigen(var(scale(xm),na.rm=TRUE,use="pairwise.complete.obs"))$values
    d <- sum(abs(e1 - e2)/e1)
    return(d)
  }
  if (method == "robeigen") {
    e1 <- eigen(covMcd(scale(x))$cov)$values
    e2 <- eigen(covMcd(scale(xm))$cov)$values
    d <- sum(abs(e1 - e2)/e1)
    return(d)
  }
}
