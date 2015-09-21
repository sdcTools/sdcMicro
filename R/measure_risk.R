#' Disclosure Risk for Categorical Variables
#'
#' The function measures the disclosure risk for weighted or unweighted data.
#' It computes the individual risk (and household risk if reasonable) and the
#' global risk. It also computes a risk threshold based on a global risk value.
#'
#' To be used when risk of disclosure for individuals within a family is
#' considered to be statistical independent.
#'
#' Internally, function \emph{freqCalc()} and \emph{indivRisk} are used for
#' estimation.
#'
#' Measuring individual risk: The individual risk approach based on so-called
#' super-population models. In such models population frequency counts are
#' modeled given a certain distribution.  The estimation procedure of sample
#' frequency counts given the population frequency counts is modeled by
#' assuming a negative binomial distribution. This is used for the estimation
#' of the individual risk. The extensive theory can be found in Skinner (1998),
#' the approximation formulas for the individual risk used is described in
#' Franconi and Polettini (2004).
#'
#' Measuring hierarchical risk: If \dQuote{hid} - the index of variable holding
#' information on the hierarchical cluster structures (e.g., individuals that
#' are clustered in households) - is provided, the hierarchical risk is
#' additional estimated.  Note that the risk of re-identifying an individual
#' within a household may also affect the probability of disclosure of other
#' members in the same household. Thus, the household or cluster-structure of
#' the data must be taken into account when estimating disclosure risks. It is
#' commonly assumed that the risk of re-identification of a household is the
#' risk that at least one member of the household can be disclosed. Thus this
#' probability can be simply estimated from individual risks as 1 minus the
#' probability that no member of the household can be identified.
#'
#' Global risk: The sum of the individual risks in the dataset gives the
#' expected number of re-identifications that serves as measure of the global
#' risk.
#'
#' l-Diversity: If \dQuote{ldiv_index} is unequal to NULL, i.e. if the indices
#' of sensible variables are specified, various measures for l-diversity are
#' calculated. l-diverstiy is an extension of the well-known k-anonymity
#' approach where also the uniqueness in sensible variables for each pattern
#' spanned by the key variables are evaluated.
#'
#' @name measure_risk
#' @aliases measure_risk-methods measure_risk,data.frame-method
#' measure_risk,matrix-method measure_risk,sdcMicroObj-method measure_risk
#' ldiversity ldiversity-methods ldiversity,data.frame-method
#' ldiversity,matrix-method ldiversity,sdcMicroObj-method print.measure_risk
#' print.ldiversity
#' @docType methods
#' @param obj Object of class \code{\link{sdcMicroObj-class}}
#' @param x Output of measure_risk() or ldiversity()

#' @param ... see arguments below
#' \itemize{
#' \item{data}{Input data, either a matrix or a data.frame.}
#' \item{keyVars}{Names of categorical key variables}
#' \item{w}{name of variable containing sample weights}
#' \item{hid}{name of the clustering variable, e.g. the household ID}
#' \item{max_global_risk}{Maximal global risk for threshold computation}
#' \item{fast_hier}{If TRUE a fast approximation is computed if household data are provided.}
#' }
#' @return A modified \code{\link{sdcMicroObj-class}} object or a list with the following elements:
#' \itemize{
#' \item{global_risk_ER}{expected number of re-identification.}
#' \item{global_risk}{global risk (sum of indivdual risks).}
#' \item{global_risk_pct}{global risk in percent.}
#' \item{Res}{matrix with the risk, frequency in the sample and grossed-up frequency in the population (and the hierachical risk) for each observation.}
#' \item{global_threshold}{for a given max_global_risk the threshold for the risk of observations.}
#' \item{max_global_risk}{the input max_global_risk of the function.}
#' \item{hier_risk_ER}{expected number of re-identification with household structure.}
#' \item{hier_risk}{global risk with household structure (sum of indivdual risks).}
#' \item{hier_risk_pct}{global risk with household structure in percent.}
#' \item{ldiverstiy}{Matrix with Distinct_Ldiversity,
#' Entropy_Ldiversity and Recursive_Ldiversity for each sensitivity variable.}}
#' @section Methods: \describe{
#' \item{list("signature(obj = \"data.frame\")")}{Method for object of class \dQuote{data.frame}}
#' \item{list("signature(obj = \"matrix\")")}{Method for object of class \dQuote{matrix}}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{Method for object of S4 class \code{\link{sdcMicroObj-class}}}
#' }
#' @author Alexander Kowarik, Bernd Prantner, Matthias Templ, minor parts of IHSN C++ source
#' @seealso \code{\link{freqCalc}}, \code{\link{indivRisk}}
#' @references Franconi, L. and Polettini, S. (2004) \emph{Individual risk
#' estimation in mu-Argus: a review}. Privacy in Statistical Databases, Lecture
#' Notes in Computer Science, 262--272. Springer
#'
#' Machanavajjhala, A. and Kifer, D. and Gehrke, J. and Venkitasubramaniam, M.
#' (2007) \emph{l-Diversity: Privacy Beyond k-Anonymity}.  ACM Trans. Knowl.
#' Discov. Data, 1(1)
#'
#' additionally, have a look at the vignettes of sdcMicro for further reading.
#' @keywords manip
#' @export
#' @examples
#'
#' ## measure_risk with sdcMicro objects:
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof','walls','water','electcon'),
#' numVars=c('expend','income','savings'), w='sampling_weight')
#'
#' ## risk is already estimated and available in...
#' names(sdc@@risk)
#'
#' ## measure risk on data frames or matrices:
#' res <- measure_risk(testdata,
#'   keyVars=c("urbrur","roof","walls","water","sex"))
#' print(res)
#' head(res$Res)
#' resw <- measure_risk(testdata,
#'   keyVars=c("urbrur","roof","walls","water","sex"),w="sampling_weight")
#' print(resw)
#' head(resw$Res)
#' res1 <- ldiversity(testdata,
#'   keyVars=c("urbrur","roof","walls","water","sex"),ldiv_index="electcon")
#' print(res1)
#' head(res1)
#' res2 <- ldiversity(testdata,
#'   keyVars=c("urbrur","roof","walls","water","sex"),ldiv_index=c("electcon","relat"))
#' print(res2)
#' head(res2)
#'
#' # measure risk with household risk
#' resh <- measure_risk(testdata,
#'   keyVars=c("urbrur","roof","walls","water","sex"),w="sampling_weight",hid="ori_hid")
#' print(resh)
#'
#' # change max_global_risk
#' rest <- measure_risk(testdata,
#'   keyVars=c("urbrur","roof","walls","water","sex"),
#'   w="sampling_weight",max_global_risk=0.0001)
#' print(rest)
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' ## already interally applied and availabe in object sdc:
#' ## sdc <- measure_risk(sdc)
#'
setGeneric("measure_risk", function(obj, ...) {
  standardGeneric("measure_risk")
})

setMethod(f = "measure_risk", signature = c("sdcMicroObj"),
  definition = function(obj, ...) {
  origData <- get.sdcMicroObj(obj, type = "origData")

  manipData <- get.sdcMicroObj(obj, type = "manipKeyVars")
  keyVars <- c(1:length(manipData))
  w <- get.sdcMicroObj(obj, type = "weightVar")
  if (length(w) > 0) {
    manipData <- cbind(manipData, origData[, w])
    w <- length(manipData)
  } else w <- NULL
  hhId <- get.sdcMicroObj(obj, type = "hhId")
  if (length(hhId) > 0) {
    manipData <- cbind(manipData, origData[, hhId])
    hhId <- length(manipData)
  } else hhId <- NULL
  res <- measure_riskWORK(manipData, keyVars, w = w, hid = hhId, ...)
  risk <- get.sdcMicroObj(obj, type = "risk")
  risk$global <- list()
  risk$global$risk <- res$global_risk
  risk$global$risk_ER <- res$global_risk_ER
  risk$global$risk_pct <- res$global_risk_pct
  risk$global$threshold <- res$global_threshold
  risk$global$max_risk <- res$max_global_risk
  if ("hier_risk" %in% names(res)) {
    risk$global$hier_risk_ER <- res$hier_risk_ER
    risk$global$hier_risk <- res$hier_risk
    risk$global$hier_risk_pct <- res$hier_risk_pct
  }
  risk$individual <- res$Res

  obj <- set.sdcMicroObj(obj, type = "risk", input = list(risk))
  obj
})

setMethod(f = "measure_risk", signature = c("data.frame"),
definition = function(obj, ...) {
  measure_riskWORK(data = obj, ...)
})

setMethod(f = "measure_risk", signature = c("matrix"),
definition = function(obj, ...) {
  measure_riskWORK(data = obj, ...)
})

measure_riskWORK <- function(data, keyVars, w = NULL, missing = -999, hid = NULL, max_global_risk = 0.01, fast_hier = TRUE) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  variables <- keyVars
  weight_variable <- w
  if ((is.null(variables) || !variables %in% colnames(data)) && is.character(variables))
    stop("Please define valid key variables") else if (is.numeric(variables)) {
    if (all(variables %in% c(1:ncol(data))))
      variables <- colnames(data)[variables] else stop("Please define valid key variables")
  }
  if (!is.null(weight_variable)) {
    if (!weight_variable %in% colnames(data) && is.character(weight_variable))
      stop("Weight variable not found!") else if (is.numeric(weight_variable)) {
      if (weight_variable %in% c(1:ncol(data)))
        weight_variable <- colnames(data)[weight_variable] else stop("Weight variable not found!")
    }
  }
  TFcharacter <- lapply(data[, keyVars, drop = FALSE], class) %in% "character"
  if (any(TFcharacter)) {
    for (kvi in which(TFcharacter)) {
      data[, keyVars[kvi]] <- as.factor(data[, keyVars[kvi]])
    }
  }

  f <- freqCalc(data, keyVars = keyVars, w = w)
  ir <- indivRisk(f, survey = !is.null(w))
  Res <- matrix(NA, ncol = 3, nrow = nrow(data))
  Res[, 1] <- ir$rk
  Res[, 2] <- ir$fk
  Res[, 3] <- f$Fk

  weighted <- 0
  if (!is.null(weight_variable)) {
    weighted <- 1
  }
  n_key_vars <- length(variables)
  dataX <- data[, c(variables), drop = FALSE]
  for (i in 1:ncol(dataX)) {
    if (!is.numeric(dataX[, i])) {
      dataX[, i] <- as.numeric(dataX[, i])
    }
  }
  if (weighted == 1) {
    dataX <- cbind(dataX, data[, weight_variable])
  }
  dataX <- as.matrix(dataX)
  ind <- do.call(order, data.frame(dataX))
  dataX <- dataX[ind, , drop = FALSE]
  ind <- order(c(1:nrow(dataX))[ind])
  # res <- .Call('measure_risk',dataX,weighted,n_key_vars,2,-99,missing)#
  res <- list()
  res$Res <- Res
  colnames(res$Res) <- c("risk", "fk", "Fk")
  res$global_risk_ER <- sum(ir$rk, na.rm = TRUE)
  res$global_risk <- res$global_risk_ER/nrow(res$Res)
  res$global_risk_pct <- res$global_risk * 100

  ind <- order(res$Res[, 1], decreasing = TRUE)
  if (max_global_risk >= 1 || max_global_risk <= 0) {
    stop("max_global_risk argument must be between 0 and 1!")
  }
  resth <- .Call("measure_threshold", res$Res[ind, 1], max_global_risk)
  if (is.na(resth$global_threshold_unsafe) || is.na(resth$global_threshold_safe) || is.na(resth$global_threshold)) {
    res[["global_threshold"]] <- NA
  } else if (resth$global_threshold_unsafe == resth$global_threshold && resth$global_threshold_safe ==
    resth$global_threshold)
    res[["global_threshold"]] <- NA else res[["global_threshold"]] <- resth$global_threshold
  res[["max_global_risk"]] <- max_global_risk
  class(res) <- "measure_risk"
  if (!is.null(hid)) {
    ind <- order(data[, hid])
    dataX <- cbind(data[, hid], res$Res[, 1])[ind, ]
    ind <- order(c(1:nrow(dataX))[ind])
    for (i in 1:ncol(dataX)) {
      if (!is.numeric(dataX[, i]))
        dataX[, i] <- as.numeric(dataX[, i])
    }
    dataX <- as.matrix(dataX)
    maxHH <- max(table(dataX[, 1]), na.rm = TRUE)
    if (fast_hier) {
      # warning('The households are to large for a fast computation of the hierachical risk.\n
      # (Use the parameter forceHier to perform the computation anyway)')
      reshier <- by(dataX[, 2], dataX[, 1], function(x) 1 - prod(1 - x), simplify = TRUE)
      reshier <- data.frame(cbind(reshier, unique(dataX[, 1])))
      colnames(reshier) = c("reshier", "hhid")
      dataX <- data.frame(dataX[, 1])
      colnames(dataX) = c("hhid")
      datX <- merge(dataX, reshier, all.x = TRUE)

      res$Res <- cbind(res$Res, datX$reshier[ind])
      res[["hier_risk_ER"]] <- sum(res$Res[, 4], na.rm = TRUE)
      res[["hier_risk"]] <- sum(res$Res[, 4], na.rm = TRUE)/nrow(res$Res)
      res[["hier_risk_pct"]] <- res[["hier_risk"]] * 100
    } else {
      resh <- .Call("measure_hierachical", dataX)
      resh$Res <- resh$Res[ind]
      res$Res <- cbind(res$Res, resh$Res)
      res[["hier_risk_ER"]] <- resh[["hier_risk_ER"]]
      res[["hier_risk"]] <- resh[["hier_risk"]]
      res[["hier_risk_pct"]] <- resh[["hier_risk_pct"]]
    }
    colnames(res$Res) <- c("risk", "fk", "Fk", "hier_risk")
  }
  invisible(res)
}

#' @rdname measure_risk
#' @export
#' @param missing a integer value to be used as missing value in the C++ routine
#' @param ldiv_index indices (or names) of the variables used for l-diversity
#' @param l_recurs_c l-Diversity Constant
#' @note internal function
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
setGeneric("ldiversity", function(obj, ldiv_index=NULL, l_recurs_c = 2, missing = -999, ...) {
  standardGeneric("ldiversity")
})

setMethod(f = "ldiversity", signature = c("sdcMicroObj"),
definition = function(obj, ldiv_index=NULL, l_recurs_c = 2, missing = -999) {
  o <- obj@origData
  k <- obj@manipKeyVars
  n <- obj@manipNumVars
  s <- obj@manipStrataVar
  ldiv_index <- ldiv_index
  if ( is.null(ldiv_index) ) {
    sensVar <- get.sdcMicroObj(obj, "sensibleVar")
    if ( is.null(sensVar) ) {
      err <- paste0("You need to specify argument 'sensibleVar' in 'createSdcObj()'")
      err <- paste0(err, " or specify it directly (argument 'ldiv_index') so that the")
      err <- paste0(err, " ldiversity risk-measure can be calculated!\n")
      stop(err)
    } else{
      ldiv_index <- sensVar
    }
  }
  if (!is.null(k))
    o[, colnames(k)] <- k
  if (!is.null(n))
    o[, colnames(n)] <- n
  if (!is.null(s))
    o$sdcGUI_strataVar <- s
  kV <- colnames(obj@origData)[get.sdcMicroObj(obj, "keyVars")]
  obj@risk$ldiversity <- ldiversityWORK(data = o, keyVars = kV, l_recurs_c = l_recurs_c, ldiv_index = ldiv_index, missing = missing)
  return(obj)
})

setMethod(f = "ldiversity", signature = c("data.frame"),
definition = function(obj, keyVars, ldiv_index, l_recurs_c = 2, missing = -999) {
  ldiversityWORK(data = obj, keyVars = keyVars, ldiv_index = ldiv_index, l_recurs_c = l_recurs_c, missing = missing)
})

setMethod(f = "ldiversity", signature = c("matrix"), definition = function(obj, keyVars, ldiv_index,
  l_recurs_c = 2, missing = -999) {
  ldiversityWORK(data = obj, keyVars = keyVars, ldiv_index = ldiv_index, l_recurs_c = l_recurs_c, missing = missing)
})

ldiversityWORK <- function(data, keyVars, ldiv_index, missing = -999, l_recurs_c = 2) {
  variables <- keyVars
  if ((is.null(variables) || !variables %in% colnames(data)) && is.character(variables))
    stop("Please define valid key variables") else if (is.numeric(variables)) {
    if (all(variables %in% c(1:ncol(data))))
      variables <- colnames(data)[variables] else stop("Please define valid key variables")
  }
  if (!is.null(ldiv_index)) {
    if (is.numeric(ldiv_index)) {
      ldiv_var <- colnames(data)[ldiv_index]
      ldiv_index <- length(variables) + 1:length(ldiv_index)
    } else if (is.character(ldiv_index)) {
      ldiv_var <- ldiv_index
      ldiv_index <- length(variables) + 1:length(ldiv_index)
    }
    if (any(ldiv_var %in% variables))
      stop("Sensitivity variable should not be a keyVariable")
  } else ldiv_var <- character(0)

  n_key_vars <- length(variables)
  dataX <- data[, c(variables, ldiv_var), drop = FALSE]
  for (i in 1:ncol(dataX)) {
    if (!is.numeric(dataX[, i]))
      dataX[, i] <- as.numeric(dataX[, i])
  }
  dataX <- as.matrix(dataX)
  ind <- do.call(order, data.frame(dataX))
  dataX <- dataX[ind, , drop = FALSE]
  ind <- order(c(1:nrow(dataX))[ind])
  if (is.null(ldiv_index))
    ldiv_index = -99
  if (length(ldiv_index) > 5)
    stop("Maximal number of sensitivity variables is 5")
  res <- .Call("measure_risk", dataX, 0, n_key_vars, l_recurs_c, ldiv_index, missing)
  res$Fk <- res$Res[, 3]
  res$Res <- res$Res[ind, ]
  if (all(ldiv_index != -99)) {
    res$Mat_Risk <- res$Mat_Risk[ind, ]
    names(res)[names(res) == "Mat_Risk"] <- "ldiversity"
    colnames(res$ldiversity) <- c(paste(rep(ldiv_var, each = 3), rep(c("Distinct_Ldiversity",
      "Entropy_Ldiversity", "Recursive_Ldiversity"), length(ldiv_index)), sep = "_"),
      "MultiEntropy_Ldiversity", "MultiRecursive_Ldiversity")
  } else {
    res <- res[names(res) != "Mat_Risk"]
  }
  ind <- order(res$Res[, 1], decreasing = TRUE)
  res <- res$ldiversity
  class(res) <- "ldiversity"
  invisible(res)
}

#' Print method for objects from class ldiversity
#'
#' Print method for objects from class ldiversity
#' @rdname measure_risk
#' @return Prints risk-information into the console
#' @author Bernhard Meindl, Matthias Templ
#' @seealso \code{\link{measure_risk}}
#' @keywords print
#' @method print measure_risk
#' @export
print.measure_risk <- function(x, ...) {
  cat("\n")
  cat("--------------------------\n")
  s <- sum(x$Res[, 1] > median(x$Res[, 1]) + 3 * mad(x$Res[, 1]) & x$Res[, 1] > 0.1)
  cat(paste(s, "obs. with higher risk as the main part\n"))
  cat("Expected no. of re-identifications:\n", round(x$global_risk_ER, 2), "")
  cat("(", round(x$global_risk_pct, 2), "%)\n")
  if (is.na(x$global_threshold))
    x$global_threshold <- Inf
  cat("Threshold:", round(x$global_threshold, 2), "\n (for maximal global risk", round(x$max_global_risk,
    2), ")\n")
  cat("--------------------------\n")
  if ("hier_risk_ER" %in% names(x)) {
    if (!is.na(x$hier_risk_ER)) {
      cat("--------------------------\n")
      cat("Hierarchical risk \n")
      cat("--------------------------\n")
      cat("Expected no. of re-identifications:\n", round(x$hier_risk_ER, 2), "")
      cat("(", round(x$hier_risk_pct, 2), "% )\n")
    } else {
      cat("--------------------------\n")
      cat("Hierarchical risk not available\n")
      cat("--------------------------\n")
    }
  }
}

#' Print method for objects from class ldiversity
#'
#' Print method for objects from class ldiversity
#' @rdname measure_risk
#' @return Information on L-Diversity Measures in the console
#' @author Bernhard Meindl, Matthias Templ
#' @seealso \code{\link{measure_risk}}
#' @keywords print
#' @method print ldiversity
#' @export
print.ldiversity <- function(x, ...) {
  cat("--------------------------\n")
  cat("L-Diversity Measures \n")
  cat("--------------------------\n")
  print(summary(x[, grep("_Distinct_Ldiversity", colnames(x))]))
}
