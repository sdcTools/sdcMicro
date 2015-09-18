#' Suda2: Detecting Special Uniques
#'
#' SUDA risk measure for data from (stratified) simple random sampling.
#'
#' Suda 2 is a recursive algorithm for finding Minimal Sample Uniques. The
#' algorithm generates all possible variable subsets of defined categorical key
#' variables and scans them for unique patterns in the subsets of variables.
#' The lower the amount of variables needed to receive uniqueness, the higher
#' the risk of the corresponding observation.
#'
#' @name suda2
#' @aliases suda2-methods suda2,data.frame-method suda2,matrix-method
#' suda2,sdcMicroObj-method suda2
#' @docType methods
#' @param obj object of class \dQuote{data.frame} or object of class \code{\link{sdcMicroObj-class}}
#' @param ... see arguments below
#' \itemize{
#' \item{variables}{Categorical (key) variables. Either the column names or and
#' index of the variables to be used for risk measurement.}
#' \item{missing}{Missing value coding in the given data set.}
#' \item{DisFraction}{It is the sampling fraction for the simple random
#' sampling, and the common sampling fraction for stratified sampling. By
#' default, it's set to 0.01.}}
#' @return A modified \code{\link{sdcMicroObj-class}} object or the following list
#' \itemize{
#' \item{ContributionPercent}{The contribution of each key variable to the SUDA
#' score, calculated for each row.}
#' \item{score }{The suda score.}
#' \item{disscore }{The dis suda score}}
#' @section Methods: \describe{#'
#' \item{list("signature(obj = \"data.frame\")")}{}
#' \item{list("signature(obj = \"matrix\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Alexander Kowarik based on the C++ code from the Organisation For
#' Economic Co-Operation And Development.
#'
#' For the C++ code: This work is being supported by the International
#' Household Survey Network and funded by a DGF Grant provided by the World
#' Bank to the PARIS21 Secretariat at the Organisation for Economic
#' Co-operation and Development (OECD). This work builds on previous work which
#' is elsewhere acknowledged.
#' @references C. J. Skinner; M. J. Elliot (20xx) A Measure of Disclosure Risk
#' for Microdata. \emph{Journal of the Royal Statistical Society: Series B
#' (Statistical Methodology)}, Vol. 64 (4), pp 855--867.
#'
#' M. J. Elliot, A. Manning, K. Mayes, J. Gurd and M. Bane (20xx) SUDA: A
#' Program for Detecting Special Uniques, Using DIS to Modify the
#' Classification of Special Uniques
#'
#' Anna M. Manning, David J. Haglin, John A. Keane (2008) A recursive search
#' algorithm for statistical disclosure assessment. \emph{Data Min Knowl Disc}
#' 16:165 -- 196
#' @keywords manip
#' @export
#' @examples
#'
#' \dontrun{
#' data(testdata2)
#' data_suda2 <- suda2(testdata2,variables=c("urbrur","roof","walls","water","sex"))
#' data_suda2
#' summary(data_suda2)
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- suda2(sdc)
#' }
#'
setGeneric("suda2", function(obj, ...) {
  standardGeneric("suda2")
})

setMethod(f = "suda2", signature = c("sdcMicroObj"),
definition = function(obj, ...) {
  manipData <- get.sdcMicroObj(obj, type = "manipKeyVars")
  keyVars <- colnames(manipData)

  risk <- get.sdcMicroObj(obj, type = "risk")
  risk$suda2 <- suda2WORK(manipData, variables = keyVars, ...)

  obj <- set.sdcMicroObj(obj, type = "risk", input = list(risk))
  obj
})

setMethod(f = "suda2", signature = c("data.frame"),
definition = function(obj, ...) {
  suda2WORK(data = obj, ...)
})

setMethod(f = "suda2", signature = c("matrix"),
definition = function(obj, ...) {
  suda2WORK(data = obj, ...)
})

suda2WORK <- function(data, variables = NULL, missing = -999, DisFraction = 0.01) {
  if (is.null(variables))
    variables <- colnames(data)
  dataX <- data[, variables, drop = FALSE]
  if (length(variables) == 2)
    dataX <- cbind(dataX, rep(1, nrow(dataX))) else if (length(variables) == 1)
    dataX <- cbind(dataX, rep(1, nrow(dataX)), rep(1, nrow(dataX)))
  for (i in 1:ncol(dataX)) {
    if (!is.numeric(dataX[, i]))
      dataX[, i] <- as.numeric(dataX[, i])
  }
  dataX <- as.matrix(dataX)
  dataX[is.na(dataX)] <- missing
  dat <- .Call("Suda2", dataX, missing, ncol(dataX), DisFraction)$Res
  if (length(variables) == 2)
    dat <- dat[, -3] else if (length(variables) == 1)
    dat <- dat[, c(-2, -3)]
  colnames(dat) <- c(paste(variables, "_contribution", sep = ""), "suda_score", "dis_suda_score")
  res <- list(contributionPercent = dat[, 1:length(variables)], score = dat[, "suda_score"],
    disScore = dat[, "dis_suda_score"])
  class(res) <- "suda2"
  if (length(variables) <= 2) {
    warn_s <- "This version of Suda2 can find MSUs only in Dataset with more than 2 variables."
    warn_s <- paste0(warn_s,"\nDummy variables have been added and the result might be wrong!")
    warning(warn_s)
  }
  invisible(res)
}

#' Print method for objects from class suda2
#'
#' Print method for objects from class suda2.
#'
#'
#' @param x an object of class suda2
#' @param \dots additional arguments passed through.
#' @return Table of dis suda scores.
#' @author Matthias Templ
#' @seealso \code{\link{suda2}}
#' @keywords print
#' @method print suda2
#' @export
#' @examples
#' \dontrun{
#' data(testdata)
#' data_suda2 <- suda2(testdata,variables=c("urbrur","roof","walls","water","sex"))
#' data_suda2
#' }
#'
print.suda2 <- function(x, ...) {
  SEQ <- seq(0, 0.7, 0.1) + .Machine$double.eps
  DISSudaScore <- paste(">", seq(0, 0.7, 0.1))
  tab <- table(cut(x$disScore, breaks = c(-1, SEQ)))
  res <- data.frame(thresholds = DISSudaScore, number = as.numeric(tab))
  cat("\n Dis suda scores table: \n")
  cat("- - - - - - - - - - - \n")
  print(res)
  cat(" - - - - - - - - - - - \n")
}
