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
#' @docType methods
#' @param obj a `data.frame` or a [sdcMicroObj-class]-object
#' @param ... see arguments below
#' - `variables` Categorical (key) variables. Either the column names or and
#' index of the variables to be used for risk measurement.
#' - `missing`: Missing value coding in the given data set.
#' - `DisFraction`: It is the sampling fraction for the simple random
#' sampling, and the common sampling fraction for stratified sampling. By
#' default, it's set to 0.01.
#' - `original_scores`: if this argument is `TRUE` (the default), the
#' suda-scores are computed as described in paper "SUDA: A Program for Detecting Special
#' Uniques" by Elliot et al., if `FALSE`, the computation of the scores
#' is slightly different as it was done in the original implementation
#' of the algorithm by the IHSN.
#' @return A modified [sdcMicroObj-class] object or the following list
#' - `ContributionPercent`: The contribution of each key variable to the SUDA
#' score, calculated for each row.
#' - `score`: The suda score
#' `disscore: The dis suda score
#' - `attribute_contributions:` a `data.frame` showing how much of the total
#' risk is contributed by each variable. This information is stored in the
#' following two variables:
#'   * `variable`: containing the name of the variable
#'   * `contribution`: contains how much risk a variable contributes to the total risk.
#' - `attribute_level_contributions`: returns risks of each attribute-level as a
#' `data.frame` with the following three columns:
#'   * `variable`: the variable name
#'   * `attribute`: holding relevant level-codes
#'   * `contribution`: contains the risk of this level within the variable.
#' @md
#' @author Alexander Kowarik and Bernhard Meindl (based on the C++ code from the Organisation For
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
#'
#' Templ, M. Statistical Disclosure Control for Microdata: Methods and Applications in R.
#' \emph{Springer International Publishing}, 287 pages, 2017. ISBN 978-3-319-50272-4.
#' \doi{10.1007/978-3-319-50272-4}
#'
#' @keywords manip
#' @rdname suda2
#' @note Since version >5.0.2, the computation of suda-scores has changed and is now by default as described in
#' the original paper by Elliot et al.
#' @export
suda2 <- function(obj, ...) {
  suda2X(obj=obj, ...)
}

setGeneric("suda2X", function(obj, ...) {
  standardGeneric("suda2X")
})

setMethod(f="suda2X", signature=c("sdcMicroObj"), definition=function(obj, ...) {
  manipData <- get.sdcMicroObj(obj, type = "manipKeyVars")
  keyVars <- colnames(manipData)

  risk <- get.sdcMicroObj(obj, type = "risk")
  risk$suda2 <- suda2WORK(manipData, variables = keyVars, ...)

  obj <- set.sdcMicroObj(obj, type = "risk", input = list(risk))
  if(length(keyVars)<=2){
    warn_s <- "This version of Suda2 can find MSUs only in Dataset with more than 2 variables."
    warn_s <- paste0(warn_s,"\nDummy variables have been added and the result might be wrong!")
    obj <- addWarning(obj, warnMsg=warn_s, method="suda2", variable=NA)
  }
  return(obj)
})

setMethod(f="suda2X", signature=c("data.frame"), definition = function(obj, ...) {
  suda2WORK(data = obj, ...)
})

suda2WORK <- function(data, variables = NULL, missing = -999, DisFraction = 0.01, original_scores=TRUE) {
  stopifnot(is.logical(original_scores))
  stopifnot(length(original_scores) == 1)

  if (is.null(variables)) {
    variables <- colnames(data)
  }
  dataX <- data[, variables, drop = FALSE]
  if (length(variables) == 2) {
    dataX <- cbind(dataX, rep(1, nrow(dataX)))
  } else if (length(variables) == 1) {
    dataX <- cbind(dataX, rep(1, nrow(dataX)), rep(1, nrow(dataX)))
  }
  for (i in seq_len(ncol(dataX))) {
    if (!is.numeric(dataX[, i])) {
      dataX[, i] <- as.numeric(dataX[, i])
    }
  }
  dataX <- as.matrix(dataX)
  dataX[is.na(dataX)] <- missing
  dat <- Suda2(dataX, missing, ncol(dataX), DisFraction, original_scores)$Res
  if (length(variables) == 2) {
    dat <- dat[, -3]
  } else if (length(variables) == 1) {
    dat <- dat[, c(-2, -3)]
  }
  colnames(dat) <- c(paste0(variables, "_contribution"), "suda_score", "dis_suda_score")
  res <- list(
    contributionPercent = dat[, 1:length(variables)],
    score = dat[, "suda_score"],
    disScore = dat[, "dis_suda_score"]
  )

  # attribute contributions
  contribs <- res$contributionPercent * res$score
  df <- data.frame(
    variable = variables,
    contribution = 100 * (colSums(contribs) / sum(res$score)),
    stringsAsFactors = FALSE
  )

  rownames(df) <- NULL
  res$attribute_contributions <- df

  # attribute level contributions
  tmp <- cbind(data[, variables, drop = FALSE], contribs)
  tots <- apply(contribs, 2, sum)
  df <- NULL
  #browser()
  for (vv in variables) {
    levs <- sort(unique(data[[vv]]))
    val <- sapply(levs, function(x) {
      100*(sum(tmp[[paste0(vv, "_contribution")]][tmp[[vv]] == x], na.rm = TRUE))
    }) / tots[[paste0(vv, "_contribution")]]
    df <- rbind(df, data.frame(
      variable = vv,
      attribute = levs,
      contribution = val,
      stringsAsFactors = FALSE
    ))
  }
  res$attribute_level_contributions <- df

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
print.suda2 <- function(x, ...) {
  SEQ <- seq(0, 0.7, 0.1) + .Machine$double.eps
  DISSudaScore <- c("== 0", "(0.0, 0.1]","(0.1, 0.2]", "(0.2, 0.3]", "(0.3, 0.4]", "(0.4, 0.5]", "(0.5, 0.6]", "(0.6, 0.7]","> 0.7")
  tab <- table(cut(x$disScore, breaks = c(-1, SEQ, Inf)))
  res <- data.frame(interval = DISSudaScore, "number of records" = as.numeric(tab))
  colnames(res) <- c("Interval", "Number of records")
  message("\nDis suda scores table: \n")
  message("- - - - - - - - - - - \n")
  print(res)
  message("- - - - - - - - - - - \n")
  message("Attribute contribution:\n")
  message("- - - - - - - - - - - \n")
  print(x$attribute_contributions)
  message("- - - - - - - - - - - \n")
}
