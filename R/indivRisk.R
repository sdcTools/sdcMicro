# Benedetti-Franconi risk E(1/F_k | f_k) for real-valued frequency counts f_k > 0.
# With F_k - f_k negative binomial (size f_k, probability p_k = f_k / F_k),
#   r_k = p^f * int_0^1 t^(f - 1) * (1 - (1 - p) * t)^(-f) dt
#       = p * int_0^Inf exp(-f * x) / (p + (1 - p) * exp(-x)) dx,
# which gives the closed forms of indivRisk() for f = 1 and f = 2. The second form is
# the one integrated: integrate() fails on the first for small p. The range is split
# at xs = log((1 - p) / p), where the integrand changes its rate of decay. For large f
# all but a share of exp(-40) of the integral lies in [0, 40 / (f - 1)]; the rest is
# dropped. The part beyond xs is rescaled by f, so that it decays at rate one.
bfRiskFrac <- function(fk, pk) {
  int <- function(h, lower, upper) {
    integrate(h, lower, upper, rel.tol = 1e-10, subdivisions = 1000L)$value
  }
  one <- function(f, p) {
    if (p >= 1) {
      return(1 / f)
    }
    q <- 1 - p
    g <- function(x) exp(-f * x) / (p + q * exp(-x))
    xs <- if (q > p) log(q / p) else 0
    upper <- if (f > 1) min(xs, 40 / (f - 1)) else xs
    head <- if (upper > 0) int(g, 0, upper) else 0
    tail <- if (upper < xs) 0 else int(function(y) g(xs + y / f) / f, 0, Inf)
    p * (head + tail)
  }
  # one integral per distinct pair (fk, pk)
  key <- sprintf("%.17g %.17g", fk, pk)
  first <- !duplicated(key)
  res <- mapply(one, fk[first], pk[first], USE.NAMES = FALSE)
  as.numeric(res[match(key, key[first])])
}

#' Individual Risk computation
#'
#' Estimation of the risk for each observation. After the risk is computed one
#' can use e.g. the function localSuppr() for the protection of values of high
#' risk.  Further details can be found at the link given below.
#'
#' S4 class sdcMicro objects are only supported by function \emph{measure_risk}
#' that also estimates the individual risk with the same method.
#'
#' The frequency counts need not be integers: with missing values in the key
#' variables and \code{0 < alpha < 1}, \code{\link{freqCalc}} returns counts
#' such as \code{fk = 1.5}. For these the risk is the same posterior mean of
#' \eqn{1/F_k} as for integer counts (\eqn{F_k - f_k} negative binomial with
#' size \eqn{f_k} and probability \eqn{p_k = f_k/F_k}),
#' \deqn{r_k = p_k^{f_k} \int_0^1 t^{f_k - 1} (1 - (1 - p_k) t)^{-f_k} dt,}
#' evaluated numerically. It reduces to the closed forms used for
#' \eqn{f_k = 1} and \eqn{f_k = 2}. Method \dQuote{approx} uses it for
#' non-integer \eqn{f_k < 2} and keeps its approximation for \eqn{f_k > 2};
#' method \dQuote{exact} uses it for all non-integer \eqn{f_k}.
#'
#' @param x object from class freqCalc
#' @param method approx (default) or exact
#' @param qual final correction factor
#' @param survey TRUE, if we have survey data and FALSE if we deal with a population.
#' @return
#' \describe{
#' \item{rk: }{ base individual risk }
#' \item{method: }{method}
#' \item{qual: }{final correction factor}
#' \item{fk: }{frequency count}
#' \item{knames: }{colnames of the key variables}}
#' @note The base individual risk method was developed by Benedetti,
#' Capobianchi and Franconi
#' @author Matthias Templ. Bug in method \dQuote{exact} fixed since version
#' 2.6.5. by Youri Baeyens.
#' @references 
#' Templ, M. and Kowarik, A. and Meindl, B. 
#' Statistical Disclosure Control for Micro-Data Using the R Package sdcMicro. 
#' \emph{Journal of Statistical Software}, \strong{67} (4), 1--36, 2015. \doi{10.18637/jss.v067.i04}
#' @seealso \code{\link{measure_risk}}, \code{\link{freqCalc}}
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
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
#' f
#' f$fk
#' f$Fk
#' ## individual risk calculation:
#' indivf <- indivRisk(f)
#' indivf$rk
#'
indivRisk <- function(x, method = "approx", qual = 1, survey = TRUE) {
  ## x ... object from freqCalc
  if (length(colnames(x$freqCalc)) > 0) {
    if (all(is.numeric(x$keyVars))) {
      knames <- colnames(x$freqCalc)[x$keyVars]
    } else if (all(is.character(x$keyVars))) {
      knames <- x$keyVars
    } else {
      stop("in 'indivRisk' all keyVars must be defined in the same way:\n as column index or as column name.")
    }
  } else {
    knames <- NULL
  }
  if (survey == TRUE) {
    P <- ncol(x$freqCalc)
    N <- dim(x$freqCalc)[1]
    fk <- x$fk
    Fk <- x$Fk
    pk <- fk/Fk
    # pk = pk-0.0001
    rk <- rep(0, N)
    # non-integer counts: freqCalc() with 0 < alpha < 1 and missing values in the keys
    frac <- is.finite(fk) & is.finite(pk) & fk > 0 & pk > 0 & pk < 1 & fk != round(fk)
    if (method == "exact") {
      A <- (pk^(1 - fk) - 1)/(fk - 1)
      B <- function(fk, pk, i) {
        (fk - 1 - i)^2/((i + 1) * (fk - 2 - i)) * (pk^(i + 2 - fk) - 1)/(pk^(i + 1 -
          fk) - 1)
      }
      BB <- function(fk, pk) {
        bb <- 0
        for (m in 0:(fk - 3)) {
          b <- 1
          for (m2 in 0:m) {
          b <- b * B(fk, pk, m2)
          }
          bb <- bb + (-1)^(m + 1) * b
        }
        bb
      }
      # r <- (pk/(1-pk)) * (A * (1 + (-1)^1 * B(0) ))
      eins <- (pk/(1 - pk))^fk
      drei <- (-1)^fk * log(pk)
      rk <- rep(0, N)
      for (k in 1:N) {
        if (frac[k]) {
          next
        }
        if (fk[k] > 2) {
          rk[k] <- eins[k] * ((A[k] * (1 + BB(fk[k], pk[k]))) + drei[k])
        }
        if (fk[k] == 2) {
          rk[k] <- (pk[k]/(1 - pk[k])) - (((pk[k]/(1 - pk[k]))^2) * log(1/pk[k]))
        }
        if (fk[k] == 1) {
          rk[k] <- (pk[k]/(1 - pk[k])) * log(1/pk[k])
        }
      }
    }
    if (method == "approx") {
      rk <- rep(0, N)
      for (k in 1:N) {
        if (fk[k] > 2) {
          rk[k] <- pk[k]/(fk[k] - (1 - pk[k]))
        }
        if (fk[k] == 2) {
          rk[k] <- (pk[k]/(1 - pk[k])) - (((pk[k]/(1 - pk[k]))^2) * log(1/pk[k]))
        }
        if (fk[k] == 1) {
          rk[k] <- (pk[k]/(1 - pk[k])) * log(1/pk[k])
        }
      }
    }
    # the branches above only cover fk == 1, fk == 2 and fk > 2: without this step a
    # count with 1 < fk < 2 kept the risk 0, and "exact" gave NaN for a non-integer fk > 2
    if (method == "approx") {
      frac <- frac & fk < 2
    }
    if (method %in% c("approx", "exact") && any(frac)) {
      rk[frac] <- bfRiskFrac(fk[frac], pk[frac])
    }
    TF <- fk == Fk
    if (any(TF)) {
      rk[TF] <- 1/fk[TF]
    }
    rk <- rk * qual
    rk <- list(rk = rk, method = method, qual = qual, fk = x$fk, knames = knames)
  }
  if (survey == FALSE) {
    rk <- list(rk = 1/x$fk, method = NA, qual = NA, fk = x$fk, knames = knames)
  }
  class(rk) <- "indivRisk"
  invisible(rk)
}

#' Print method for objects from class indivRisk
#'
#' Print method for objects from class indivRisk
#'
#' @param x object from class indivRisk
#' @param \dots Additional arguments passed through.
#' @return few information about the method and the final correction factor for
#' objects of class \sQuote{indivRisk}.
#' @author Matthias Templ
#' @seealso \code{\link{indivRisk}}
#' @keywords print
#' @method print indivRisk
#' @export
#' @examples
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' f1 <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
#' data.frame(fk=f1$fk, Fk=f1$Fk)
#' ## individual risk calculation:
#' indivRisk(f1)
#'
print.indivRisk <- function(x, ...) {
  # message('\n ----- individual risk ----- \n')
  message(paste("method=", x$method, ", qual=", x$qual, sep = ""))
  message("\n --------------------------- \n")
  s <- sum(x$rk > median(x$rk) + 3 * mad(x$rk) & x$rk > 0.1)
  message(paste(s, "obs. with high risk"))
}

