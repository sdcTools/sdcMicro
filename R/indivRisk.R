#' Individual Risk computation
#'
#' Individual risk computation.
#'
#' Estimation of the risk for each observation. After the risk is computed one
#' can use e.g. the function localSuppr() for the protection of values of high
#' risk.  Further details can be found at the link given below.
#'
#' S4 class sdcMicro objects are only supported by function \emph{measure_risk}
#' that also estimates the individual risk with the same method.
#'
#' @param x object from class freqCalc
#' @param method approx (default) or exact
#' @param qual final correction factor
#' @param survey TRUE, if we have survey data and FALSE if we deal with a population.
#' @return
#' \itemize{
#' \item{rk}{ base individual risk }
#' \item{method }{method}
#' \item{qual}{final correction factor}
#' \item{fk}{frequency count}
#' \item{knames}{colnames of the key variables}}
#' @note The base individual risk method was developed by Benedetti,
#' Capobianchi and Franconi
#' @author Matthias Templ. Bug in method \dQuote{exact} fixed since version
#' 2.6.5. by Youri Baeyens.
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
#' f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
#' f
#' f$fk
#' f$Fk
#' ## individual risk calculation:
#' indivRisk(f)
#'
print.indivRisk <- function(x, ...) {
  # cat('\n ----- individual risk ----- \n')
  cat(paste("method=", x$method, ", qual=", x$qual, sep = ""))
  cat("\n --------------------------- \n")
  s <- sum(x$rk > median(x$rk) + 3 * mad(x$rk) & x$rk > 0.1)
  cat(paste(s, "obs. with high risk"))
}

