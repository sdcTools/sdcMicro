#' Rank Swapping
#'
#' Swapping values within a range so that, first, the correlation structure of
#' original variables are preserved, and second, the values in each record are
#' disturbed.  To be used on numeric or ordinal variables where the rank can be
#' determined and the correlation coefficient makes sense.
#'
#' Rank swapping sorts the values of one numeric variable by their numerical
#' values (ranking).  The restricted range is determined by the rank of two
#' swapped values, which cannot differ by more than a proportion \code{P} of
#' the number of swappable records. Missing values and the values grouped by
#' \code{TopPercent} and \code{BottomPercent} are not swapped and do not count
#' as swappable records. Only non-negative P, R0 and K0 are used and only one
#' of them must be supplied. If none is supplied, sdcMicro sets parameter R0 to
#' 0.95 internally. If \code{R0} or \code{K0} is supplied, the rank range is
#' derived from it for each variable.
#'
#' Note that \code{P} is a proportion, whereas \code{TopPercent} and
#' \code{BottomPercent} are percentages: a 10\% rank range is \code{P = 0.1},
#' while 5\% top coding is \code{TopPercent = 5}. Values of \code{P} larger
#' than 1 do not restrict the rank range at all and raise a warning.
#'
#' @name rankSwap
#' @docType methods
#' @param obj a \code{\link{sdcMicroObj-class}}-object or a \code{data.frame}
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
#' can specify the preservation factor as \eqn{R_0=\frac{R_1}{R_2}}{R_0 =
#' R_1/R_2} where \eqn{R_1}{R_1} is the correlation coefficient of the two
#' fields after swapping, and \eqn{R_2}{R_2} is the correlation coefficient of
#' the two fields before swapping.
#' @param P Rank range as a proportion (not a percentage) of the number of
#' swappable records, a value between 0 and 1; use \code{P = 0.1} for a 10\%
#' rank range. For the value at rank \eqn{j}{j} among the swappable values, the
#' swap partner is drawn at random from the not yet swapped values at ranks
#' \eqn{k}{k} with \eqn{j < k < j + P N_s}{j < k < j + P*N_s}, where
#' \eqn{N_s}{N_s} is the number of swappable records: the number of
#' observations minus missing values and the values grouped by
#' \code{TopPercent} and \code{BottomPercent}.
#' @param missing missing - the value to be used as missing value
#' in the C++ routine instead of NA. If NA, a suitable value is calculated internally.
#' Note that in the returned dataset, all NA-values (if any) will be replaced with
#' this value.
#' @param seed Seed.
#' @return The rank-swapped data set or a modified \code{\link{sdcMicroObj-class}} object.
#' @author Alexander Kowarik for the interface, Bernhard Meindl for improvements.
#'
#' For the underlying C++ code: This work is being supported by the
#' International Household Survey Network (IHSN) and funded by a DGF Grant
#' provided by the World Bank to the PARIS21 Secretariat at the Organisation
#' for Economic Co-operation and Development (OECD).  This work builds on
#' previous work which is elsewhere acknowledged.
#' @references
#' Moore, Jr.R. (1996) Controlled data-swapping techniques for
#' masking public use microdata, U.S. Bureau of the Census \emph{Statistical
#' Research Division Report Series}, RR 96-04.
#'
#' Kowarik, A. and Templ, M. and Meindl, B. and Fonteneau, F. and Prantner, B.:
#' \emph{Testing of IHSN Cpp Code and Inclusion of New Methods into sdcMicro},
#' in: Lecture Notes in Computer Science, J. Domingo-Ferrer, I. Tinnirello
#' (editors.); Springer, Berlin, 2012, ISBN: 978-3-642-33626-3, pp. 63-77.
#' \doi{10.1007/978-3-642-33627-0_6}
#'
#' @export
#' @examples
#' data(testdata2)
#' data_swap <- rankSwap(
#'   obj = testdata2,
#'   variables = c("age", "income", "expend", "savings")
#' )
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(
#'   dat = testdata2,
#'   keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
#'   numVars = c("expend", "income", "savings"),
#'   w = "sampling_weight")
#' sdc <- rankSwap(sdc)
rankSwap <- function(obj, variables = NULL, TopPercent = 5, BottomPercent = 5,
                     K0 = NULL, R0 = NULL, P = NULL, missing = NA, seed = NULL) {

  TFpar <- c(!is.null(P), !is.null(R0), !is.null(K0))
  if (sum(TFpar) > 1) {
    stop("Only one of the parameters P, R0 and K0 should be provided.", call. = FALSE)
  }

  if (sum(TFpar) == 0) {
    R0 <- 0.95
    message("setting parameter R0 = 0.95 as no inputs have been specified.")
  }

  rankSwapX(
    obj = obj,
    variables = variables,
    TopPercent = TopPercent,
    BottomPercent = BottomPercent,
    K0 = K0,
    R0 = R0,
    P = P,
    missing = missing,
    seed = seed
  )
}

setGeneric("rankSwapX", function(obj, variables = NULL, TopPercent = 5, BottomPercent = 5,
                                 K0 = -1, R0 = 0.95, P = 0, missing = NA, seed = NULL) {
  standardGeneric("rankSwapX")
})

setMethod(f = "rankSwapX", signature = c("sdcMicroObj"),
  definition = function(obj,
                        variables = NULL,
                        TopPercent = 5,
                        BottomPercent = 5,
                        K0 = NULL,
                        R0 = 0.95,
                        P = NULL,
                        missing = NA,
                        seed = NULL) {
    manipData <- get.sdcMicroObj(obj, type = "manipNumVars")

    if (is.null(variables)) {
      variables <- colnames(manipData)
    }

    res <- rankSwap(
      obj = manipData,
      variables = variables,
      TopPercent = TopPercent,
      BottomPercent = BottomPercent,
      K0 = K0,
      R0 = R0,
      P = P,
      missing = missing,
      seed = seed
    )

    obj <- nextSdcObj(obj)
    obj <- set.sdcMicroObj(obj, type = "manipNumVars", input = list(res))
    obj <- dRisk(obj)
    obj <- dUtility(obj)
    obj
  }
)

setMethod(
  f = "rankSwapX",
  signature = c("data.frame"),
  definition = function(obj,
                        variables = NULL,
                        TopPercent = 5,
                        BottomPercent = 5,
                        K0 = -1,
                        R0 = 0.95,
                        P = 0,
                        missing = NA,
                        seed = NULL) {

  # by default, all variables will be used
  if (is.null(variables)) {
    variables <- colnames(obj)
  }
  dataX <- obj[, variables, drop = FALSE]
  dataX <- as.matrix(dataX)

  if (!all(apply(dataX, 2, is.numeric))) {
    dataX <- apply(dataX, 2, as.numeric)
  }
  data2 <- dataX
  data2[, ] <- NA

  index_missing <- is.na(dataX)
  miss_val <- ifelse(is.na(missing), min(dataX, na.rm = TRUE) - 1, missing)
  if (sum(index_missing) > 0) {
    dataX[index_missing] <- miss_val
  }

  if (!is.null(P) && !identical(P, -1)) {
    if (!is.numeric(P) || length(P) != 1 || is.na(P) || P < 0) {
      stop("'P' must be a single non-negative number, the rank range as a ",
           "proportion of the swappable records (e.g. P = 0.1 for 10%).",
           call. = FALSE)
    }
    if (P > 1) {
      warning("'P' is a proportion, not a percentage (P = 0.1 is a 10% rank ",
              "range). P = ", P, " does not restrict the rank range: any two ",
              "swappable values may be swapped.", call. = FALSE)
    }
  }

  seed <- ifelse(is.null(seed), -1L, as.integer(seed))
  if (is.null(K0)) {
    K0 <- -1
  }
  if (is.null(R0)) {
    R0 <- -1
  }
  if (is.null(P)) {
    P <- -1
  }
  dat <- RankSwap(
    dataX,
    data2,
    miss_val,
    TopPercent,
    BottomPercent,
    K0,
    R0,
    P,
    seed)$Res
  if (sum(index_missing) > 0 & is.na(missing)) {
    dat[dat == miss_val] <- NA
  }
  attributes(dat) <- NULL
  obj[, variables] <- dat
  invisible(obj)
})
