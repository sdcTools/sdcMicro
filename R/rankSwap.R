#' Rank Swapping
#'
#' Swapping values within a range so that, first, the correlation structure of
#' original variables are preserved, and second, the values in each record are
#' disturbed.  To be used on numeric or ordinal variables where the rank can be
#' determined and the correlation coefficient makes sense.
#'
#' Rank swapping sorts the values of one numeric variable by their numerical
#' values (ranking).  The restricted range is determined by the rank of two
#' swapped values, which cannot differ, by definition, by more than \eqn{P}{P}
#' percent of the total number of observations.  R0 and K0 are only used if
#' positive. Only one of the two are used (R0 is prefered if both are
#' positive).
#'
#' @name rankSwap
#' @aliases rankSwap-methods rankSwap,data.frame-method rankSwap,matrix-method
#' rankSwap,sdcMicroObj-method rankSwap
#' @docType methods
#' @param obj object of class sdcMicroObj or matrix or data frame
#' @param variables names or index of variables for that rank swapping is
#' applied.  For an object of class \code{\link{sdcMicroObj-class}}, all numeric key variables are
#' selected if variables=NULL.
#' @param TopPercent Percentage of largest values that are grouped together
#' before rank swapping is applied.
#' @param BottomPercent Percentage of lowest values that are grouped together
#' before rank swapping is applied.
#' @param K0 Subset-mean preservation factor. Preserves the means before and
#' after rank swapping within a range based on K0.  K0 is the subset-mean
#' preservation factor such that \eqn{| X_1 -X_2 | \leq \frac{2 K_0
#' X_1}{\sqrt(N_S)}}{abs(X_1-X_2<=2*K_0*X_1/sqrt (N_S)}, where \eqn{X_1}{X_1}
#' and \eqn{X_2}{X_2} are the subset means of the field before and after
#' swapping, and \eqn{N_S}{N_S} is the sample size of the subset.
#' @param R0 Multivariate preservation factor. Preserves the correlation
#' between variables within a certain range based on the given constant R0.  We
#' can specify the preservation factor as \eqn{R_0 = \frac{R_1}{R_2}}{R_0 =
#' R_1/R_2} where \eqn{R_1}{R_1} is the correlation coefficient of the two
#' fields after swapping, and \eqn{R_2}{R_2} is the correlation coefficient of
#' the two fields before swapping.
#' @param P Rank range as percentage of total sample size. We can specify the
#' rank range itself directly, noted as \eqn{P}{P}, which is the percentage of
#' the records. So two records are eligible for swapping if their ranks,
#' \eqn{i}{i} and \eqn{j}{j} respectively, satisfy \eqn{| i-j | \le \frac{P
#' N}{100}}{abs(i-j)<P*N/100}, where \eqn{N}{N} is the total sample size.
#' @param missing missing - the value to be used as missing value
#' in the C++ routine instead of NA. If NA, a suitable value is calculated internally.
#' Note that in the returned dataset, all NA-values (if any) will be replaced with
#' this value.
#' @param seed Seed.
#' @return The rank-swapped data set or a modified \code{\link{sdcMicroObj-class}} object.
#' @section Methods: \describe{
#' \item{list("signature(obj = \"data.frame\")")}{}
#' \item{list("signature(obj = \"matrix\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Alexander Kowarik for the interface, Bernhard Meindl for improvements.
#'
#' For the underlying C++ code: This work is being supported by the
#' International Household Survey Network (IHSN) and funded by a DGF Grant
#' provided by the World Bank to the PARIS21 Secretariat at the Organisation
#' for Economic Co-operation and Development (OECD).  This work builds on
#' previous work which is elsewhere acknowledged.
#' @references Moore, Jr.R. (1996) Controlled data-swapping techniques for
#' masking public use microdata, U.S. Bureau of the Census \emph{Statistical
#' Research Division Report Series}, RR 96-04.
#' @export
#' @examples
#' data(testdata2)
#' data_swap <- rankSwap(testdata2,variables=c("age","income","expend","savings"))
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- rankSwap(sdc)
setGeneric("rankSwap", function(obj, variables = NULL, TopPercent = 5, BottomPercent = 5,
  K0 = -1, R0 = 0.95, P = 0, missing = NA, seed = NULL) {
  standardGeneric("rankSwap")
})

setMethod(f = "rankSwap", signature = c("sdcMicroObj"),
definition = function(obj, variables = NULL, TopPercent = 5, BottomPercent = 5, K0 = -1,
  R0 = 0.95, P = 0, missing = NA, seed = NULL) {

  manipData <- get.sdcMicroObj(obj, type = "manipNumVars")

  if ( is.null(variables) ) {
    variables <- colnames(manipData)
  }

  res <- rankSwap(manipData, variables = variables, TopPercent = TopPercent,
    BottomPercent = BottomPercent, K0 = K0, R0 = R0, P = P, missing = missing, seed = seed)

  obj <- nextSdcObj(obj)
  obj <- set.sdcMicroObj(obj, type = "manipNumVars", input = list(res))
  obj <- dRisk(obj)
  obj <- dUtility(obj)
  obj
})

setMethod(f = "rankSwap", signature = c("matrix"),
definition = function(obj, variables = NULL, TopPercent = 5, BottomPercent = 5, K0 = -1,
  R0 = 0.95, P = 0, missing = NA, seed = NULL) {
  obj <- as.data.frame(obj)
  rankSwap(obj, variables = variables, TopPercent = TopPercent,
    BottomPercent = BottomPercent, K0 = K0, R0 = R0, missing = missing, seed = seed)
})

setMethod(f = "rankSwap", signature = c("data.frame"),
definition = function(obj, variables = NULL, TopPercent = 5, BottomPercent = 5, K0 = -1,
  R0 = 0.95, P = 0, missing = NA, seed = NULL) {

  # by default, all variables will be used
  if ( is.null(variables) ) {
    variables <- colnames(obj)
  }
  dataX <- obj[, variables]
  dataX <- as.matrix(dataX)

  if (!all(apply(dataX, 2, is.numeric))) {
    dataX <- apply(dataX, 2, as.numeric)
  }

  data2 <- dataX
  data2[, ] <- NA

  index_missing <- is.na(dataX)
  miss_val <- ifelse(is.na(missing), min(dataX, na.rm=TRUE)-1, missing)
  if ( sum(index_missing) > 0 ) {
    dataX[index_missing] <- miss_val
  }

  seed <- ifelse(is.null(seed), -1L, as.integer(seed))
  dat <- .Call("RankSwap", dataX, data2, miss_val, TopPercent, BottomPercent, K0, R0, P, seed)$Res
  if ( sum(index_missing) > 0 & is.na(missing) ) {
    dat[dat==miss_val] <- NA
  }
  obj[, variables] <- dat
  invisible(obj)
})
