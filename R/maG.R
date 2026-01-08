## Fuer data.frame, kNN-Suche auf Basis der Gowerdistance und Aggregation mit numFun bzw.
## catFun
maGowerWORK <- function(data, variables=colnames(data), aggr=3, dist_var=variables, by=NULL,
  mixed=NULL, mixed.constant=NULL, trace=FALSE, weights=NULL, numFun=mean,
  catFun=VIM::sampleCat, addRandom=FALSE) {
  data$OriginalSortingVariableMaGowerWork <- 1:nrow(data)
  if (!is.null(by)) {
    if (!all(by %in% colnames(data)))
      stop("'by'-variable not found in data set.\n'by' should be a vector of\n              variable names to split the dataset before \n applying microaggregation.")
    splfac <- as.factor(apply(data[, by, drop=FALSE], 1, function(x) paste(as.character(x),
      collapse="--")))
    spl <- split(data, splfac)
  } else {
    splfac <- as.factor(rep(1, nrow(data)))
    spl <- list(data)
  }
  orders <- vector()
  for (i in 1:ncol(data)) {
    orders <- c(orders, is.ordered(data[, i]))
  }
  orders <- colnames(data)[orders]
  levOrders <- vector()
  if (length(orders) > 0) {
    for (i in 1:length(orders)) {
      levOrders[i] <- levels(data[, orders[i]])[length(levels(data[, orders[i]]))]
    }
  }
  factors <- vector()
  for (i in 1:ncol(data)) {
    factors <- c(factors, is.factor(data[, i]))
  }
  factors <- colnames(data)[factors]
  factors <- factors[!factors %in% orders]
  numerical <- vector()
  for (i in 1:ncol(data)) {
    numerical <- c(numerical, is.numeric(data[, i]) | is.integer(data[, i]))
  }
  numerical <- colnames(data)[numerical]
  numerical <- numerical[!numerical %in% mixed]
  if (trace) {
    message("Detected as categorical variable:\n")
    print(factors)
    message("Detected as ordinal variable:\n")
    print(orders)
    message("Detected as numerical variable:\n")
    print(numerical)
  }
  if (is.null(weights)) {
    weights <- rep(1, length(dist_var))
  } else if (length(weights) != length(dist_var)) {
    stop("length of weights must be equal the number of distance variables")
  }
  if (addRandom) {
    numerical <- c(numerical, "RandomVariableForImputation")
    data[, "RandomVariableForImputation"] <- stats::rnorm(nrow(data))
    if (is.list(dist_var)) {
      for (i in 1:length(dist_var)) {
        dist_var[[i]] <- c(dist_var[[i]], "RandomVariableForImputation")
        weights[[i]] <- c(weights[[i]], min(weights[[i]])/(sum(weights[[i]]) + 1))
      }
    } else {
      dist_var <- c(dist_var, "RandomVariableForImputation")
      weights <- c(weights, min(weights)/(sum(weights) + 1))
    }
  }
  numericalX <- numerical[numerical %in% dist_var]
  factorsX <- factors[factors %in% dist_var]
  ordersX <- orders[orders %in% dist_var]
  levOrdersX <- levOrders[orders %in% dist_var]
  # print(levOrdersX)
  mixedX <- mixed[mixed %in% dist_var]
  if (is.null(mixed.constant))
    mixed.constant <- rep(0, length(mixedX))
  spl <- lapply(spl, function(dataSpl) {
    erg <- VIM::gowerD(dataSpl[, dist_var],dataSpl[, dist_var],weights=weights,numericalX,
                 factorsX,ordersX,mixedX,levOrdersX,mixed.constant=mixed.constant,returnIndex=TRUE,
                 nMin=as.integer(aggr));

    if (addRandom)
      dataSpl <- dataSpl[, -which(colnames(dataSpl) == "RandomVariableForImputation")]
    dataSpl[, variables] <- do.call("rbind", apply(erg$ind, 2, function(x) {
      as.data.frame(lapply(dataSpl[x, variables], function(x) {
        if (is.factor(x))
          return(catFun(x)) else return(numFun(x))
      }))
    }))
    return(dataSpl)
  })
  rn <- row.names(data)
  data <- do.call("rbind", spl)
  data <- data[order(data$OriginalSortingVariableMaGowerWork),]
  data$OriginalSortingVariableMaGowerWork <- NULL
  row.names(data) <- rn
  return(data)
}
#'
#' Microaggregation for numerical and categorical key variables based on a
#' distance similar to the Gower Distance
#'
#' The microaggregation is based on the distances computed similar to the Gower
#' distance. The distance function makes distinction between the variable types
#' factor,ordered,numerical and mixed (semi-continuous variables with a fixed
#' probability mass at a constant value e.g. 0)
#'
#' The function sampleCat samples with probabilities corresponding to the
#' occurrence of the level in the NNs. The function maxCat chooses the level
#' with the most occurrences and random if the maximum is not unique.
#'
#' @name microaggrGower
#' @docType methods
#' @param obj \code{\link{sdcMicroObj-class}}-object or a \code{data.frame}
#' @param variables character vector with names of variables to be aggregated
#' (Default for sdcMicroObj is all keyVariables and all numeric key variables)
#' @param aggr aggregation level (default=3)
#' @param dist_var character vector with variable names for distance
#' computation
#' @param by character vector with variable names to split the dataset before
#' performing microaggregation (Default for sdcMicroObj is strataVar)
#' @param mixed character vector with names of mixed variables
#' @param mixed.constant numeric vector with length equal to mixed, where the
#' mixed variables have the probability mass
#' @param trace TRUE/FALSE for some console output
#' @param weights numerical vector with length equal the number of variables
#' for distance computation
#' @param numFun function: to be used to aggregated numerical variables
#' @param catFun function: to be used to aggregated categorical variables
#' @param addRandom TRUE/FALSE if a random value should be added for the
#' distance computation.
#' @return The function returns the updated sdcMicroObj or simply an altered
#' data frame.
#' @note In each by group all distance are computed, therefore introducing more
#' by-groups significantly decreases the computation time and memory
#' consumption.
#' @seealso \code{\link[VIM]{sampleCat}} and \code{\link[VIM]{maxCat}}
#' @author Alexander Kowarik
#' @export microaggrGower
#' @examples
#'
#' data(testdata,package="sdcMicro")
#' testdata <- testdata[1:200,]
#' \donttest{
#' for(i in c(1:7,9)) testdata[,i] <- as.factor(testdata[,i])
#' test <- microaggrGower(testdata,variables=c("relat","age","expend"),
#'   dist_var=c("age","sex","income","savings"),by=c("urbrur","roof"))
#'
#' for(i in c(1:7,9)) testdata[,i] <- as.ordered(testdata[,i])
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#'
#' sdc <- microaggrGower(sdc)
#' }

microaggrGower <- function(obj, variables=NULL, aggr=3, dist_var=NULL, by=NULL,
  mixed=NULL, mixed.constant=NULL, trace=FALSE, weights=NULL, numFun=mean,
  catFun=VIM::sampleCat, addRandom=FALSE) {
  microaggrGowerX(obj=obj, variables=variables, aggr=aggr, dist_var=dist_var, by=by,
    mixed=mixed, mixed.constant=mixed.constant, trace=trace, weights=weights, numFun=numFun,
    catFun=catFun, addRandom=addRandom)
}

setGeneric("microaggrGowerX", function(obj, variables=NULL, aggr=3, dist_var=NULL, by=NULL,
  mixed=NULL, mixed.constant=NULL, trace=FALSE, weights=NULL, numFun=mean,
  catFun=VIM::sampleCat, addRandom=FALSE) {

  standardGeneric("microaggrGowerX")
})

setMethod(f="microaggrGowerX", signature=c("sdcMicroObj"),
  definition=function(obj, variables=NULL, aggr=3, dist_var=NULL, by=NULL, mixed=NULL,
  mixed.constant=NULL, trace=FALSE, weights=NULL, numFun=mean, catFun=VIM::sampleCat, addRandom=FALSE) {
  o <- extractManipData(obj)
  if (is.null(by) && "sdcGUI_strataVar" %in% colnames(o))
    by <- "sdcGUI_strataVar"
  nV <- get.sdcMicroObj(obj, type="numVars")
  kV <- get.sdcMicroObj(obj, type="keyVars")
  if (is.null(variables))
    variables <- colnames(o)[c(nV, kV)]
  if (is.null(dist_var))
    dist_var <- variables
  
  res <- maGowerWORK(o, variables=variables, aggr=aggr, dist_var=dist_var, by=by,
    mixed=mixed, mixed.constant=mixed.constant, trace=trace, weights=weights, numFun=numFun,
    catFun=catFun, addRandom=addRandom)
  obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(as.data.frame(res[, nV])))
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(as.data.frame(res[, kV])))
  obj <- dRisk(obj)
  obj <- dUtility(obj)
  obj <- calcRisks(obj)
  obj
})

setMethod(f="microaggrGowerX", signature=c("data.frame"),
  definition=function(obj, variables=colnames(data), aggr=3, dist_var=variables, by=NULL,
  mixed=NULL, mixed.constant=NULL, trace=FALSE, weights=NULL, numFun=mean,
  catFun=VIM::sampleCat, addRandom=FALSE) {

  maGowerWORK(data=obj, variables=variables, dist_var=dist_var, by=by, mixed=mixed,
    mixed.constant=mixed.constant, trace=trace, weights=weights, numFun=numFun,
    catFun=catFun, addRandom=addRandom)
})
