#' Local recoding via Edmond's maximum weighted matching algorithm
#'
#' To be used on both categorical and numeric input variables, although usage
#' on categorical variables is the focus of the development of this software.
#'
#' Each record in the data represents a category of the original data, and
#' hence all records in the input data should be unique by the N Input
#' Variables. To achieve bigger category sizes (k-anoymity), one can form new
#' categories based on the recoding result and repeatedly apply this algorithm.
#'
#' @name LocalRecProg
#' @docType methods
#' @param obj a \code{data.frame} or a \code{\link{sdcMicroObj-class}}-object
#' @param ancestors Names of ancestors of the cateorical variables
#' @param ancestor_setting For each ancestor the corresponding categorical variable
#' @param k_level Level for k-anonymity
#' @param FindLowestK requests the program to look for the smallest k that
#' results in complete matches of the data.
#' @param weight A weight for each variable (Default=1)
#' @param lowMemory Slower algorithm with less memory consumption
#' @param missingValue The output value for a suppressed value.
#' @param ... see arguments below
#' \describe{
#' \item{categorical}{Names of categorical variables}
#' \item{numerical}{Names of numerical variables}}
#' @return dataframe with original variables and the supressed variables
#' (suffix _lr). / the modified \code{\link{sdcMicroObj-class}}
#' @section Methods: \describe{
#' \item{list("signature(obj=\"sdcMicroObj\")")}{}}
#' @author Alexander Kowarik, Bernd Prantner, IHSN C++ source, Akimichi Takemura
#' @references
#' Kowarik, A. and Templ, M. and Meindl, B. and Fonteneau, F. and Prantner, B.:
#' \emph{Testing of IHSN Cpp Code and Inclusion of New Methods into sdcMicro},
#' in: Lecture Notes in Computer Science, J. Domingo-Ferrer, I. Tinnirello
#' (editors.); Springer, Berlin, 2012, ISBN: 978-3-642-33626-3, pp. 63-77.
#' \doi{10.1007/978-3-642-33627-0_6}
#' @keywords manip
#' @export
#' @examples
#' data(testdata2)
#' cat_vars <- c("urbrur", "roof", "walls", "water", "sex", "relat")
#' anc_var <- c("water2", "water3", "relat2")
#' anc_setting <- c("water","water","relat")
#' \donttest{
#' r1 <- LocalRecProg(
#'   obj = testdata2,
#'   categorical = cat_vars,
#'   missingValue = -99)
#' r2 <- LocalRecProg(
#'   obj = testdata2,
#'   categorical = cat_vars,
#'   ancestor = anc_var,
#'   ancestor_setting = anc_setting,
#'   missingValue = -99)
#' r3 <- LocalRecProg(
#'   obj = testdata2,
#'   categorical = cat_vars,
#'   ancestor = anc_var,
#'   ancestor_setting = anc_setting,
#'   missingValue = -99,
#'   FindLowestK = FALSE)
#'
#' # for objects of class sdcMicro:
#' sdc <- createSdcObj(
#'   dat = testdata2,
#'   keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
#'   numVars = c("expend", "income", "savings"),
#'   w = "sampling_weight")
#' sdc <- LocalRecProg(sdc)
#' }
LocalRecProg <- function(obj, ancestors=NULL, ancestor_setting=NULL, k_level=2,
  FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA, ...) {
  LocalRecProgX(obj=obj, ancestors=ancestors, ancestor_setting=ancestor_setting, k_level=k_level,
    FindLowestK=FindLowestK, weight=weight, lowMemory=lowMemory, missingValue=missingValue, ...)
}

setGeneric("LocalRecProgX", function(obj, ancestors=NULL, ancestor_setting=NULL, k_level=2,
  FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA, ...) {
  standardGeneric("LocalRecProgX")
})

setMethod(f="LocalRecProgX", signature=c("sdcMicroObj"),
definition=function(obj, ancestors=NULL, ancestor_setting=NULL, k_level=2,
  FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA, ...) {

  keyVars <- get.sdcMicroObj(obj, type="manipKeyVars")
  manipData <- keyVars
  keyVarIndices <- colnames(manipData)
  numVars <- get.sdcMicroObj(obj, type="manipNumVars")
  if (!is.null(numVars)) {
    manipData <- cbind(manipData, numVars)
    numVarIndices <- colnames(numVars)
  } else numVarIndices <- NULL
  kVlr <- paste(colnames(manipData), "_lr", sep="")
  kV <- colnames(manipData)
  if (!is.null(ancestors)) {
    if (!all(ancestors %in% colnames(manipData))) {
      origData <- get.sdcMicroObj(obj, type="origData")
      manipData <- cbind(manipData, origData[, ancestors[!ancestors %in% colnames(manipData)]])
    }
  }
  res <- LocalRecProgWORK(manipData, keyVarIndices, numerical=numVarIndices, ancestors=ancestors,
    ancestor_setting=ancestor_setting, k_level=k_level, FindLowestK=FindLowestK,
    weight=weight, lowMemory=lowMemory, missingValue=missingValue)

  newData <- res[, kVlr]
  colnames(newData) <- kV
  obj <- nextSdcObj(obj)
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(newData[, keyVarIndices]))
  if (!is.null(numVarIndices)) {
    obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(newData[, numVarIndices]))
  }
  obj <- calcRisks(obj)
  obj
})

setMethod(f="LocalRecProgX", signature=c("data.frame"),
definition=function(obj, ancestors=NULL, ancestor_setting=NULL, k_level=2,
  FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA, categorical, numerical=NULL) {

  LocalRecProgWORK(data=obj, categorical=categorical, numerical=numerical, ancestors=ancestors,
    ancestor_setting=ancestor_setting, k_level=k_level, FindLowestK=FindLowestK,
    weight=weight, lowMemory=lowMemory, missingValue=missingValue)
})

LocalRecProgWORK <- function(data, categorical, numerical=NULL, ancestors=NULL, ancestor_setting=NULL,
  k_level=2, FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA) {
  range=TRUE
  if (!is.null(ancestors) && !is.null(ancestor_setting)) {
    s1 <- which(categorical %in% ancestor_setting) - 1
    s2 <- c()
    for (ca in categorical[which(categorical %in% ancestor_setting)]) {
      s2 <- c(s2, sum(ca == ancestor_setting))
    }
    ancestor_settings <- cbind(s1, s2)
  } else if (!is.null(ancestors) && is.null(ancestor_setting))
    stop("Please specify ancestor_setting if you want to use ancestors!\n")

  if (is.null(ancestors))
    ancestor_settings <- matrix()
  if (is.null(weight))
    weight <- rep(1, length(categorical) + length(numerical))
  if (length(weight) != (length(categorical) + length(numerical)))
    stop("Length of weights must equal number of categorical variables plus number of numerical variables!\n")
  weight <- cbind(weight, c(rep(1, length(categorical)), rep(0, length(numerical))))

  dataX <- as.matrix(data[, c(categorical, numerical, ancestors), drop=FALSE])

  res <- .Call("LocalRecProg_cpp", dataX, k_level, FindLowestK, ancestor_settings, weight, range,
    FALSE, lowMemory, missingValue)
  colnames(res$Res) <- paste(c(categorical, numerical), "_lr", sep="")
  cbind(data[, c(categorical, numerical)], res$Res)
}
