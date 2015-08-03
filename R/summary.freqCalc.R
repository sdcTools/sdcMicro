#' Summary method for objects from class freqCalc
#'
#' Summary method for objects of class \sQuote{freqCalc} to provide information
#' about local suppressions.
#'
#' Shows the amount of local suppressions on each variable in which local
#' suppression was applied.
#'
#' @param object object from class freqCalc
#' @param \dots Additional arguments passed through.
#' @return Information about local suppression in each variable (only if a
#' local suppression is already done).
#' @author Matthias Templ
#' @seealso \code{\link{freqCalc}}
#' @keywords print
#' @method summary freqCalc
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
#' ## Local Suppression
#' localS <- localSupp(f, keyVar=2, indivRisk=indivf$rk, threshold=0.25)
#' f2 <- freqCalc(localS$freqCalc, keyVars=c(4,5,6), w=8)
#' summary(f2)
#'
summary.freqCalc <- function(object, ...) {
  a1 <- c(apply(object$freqCalc[, object$keyVars, drop = FALSE], 2, function(x) {
    sum(is.na(x))
  }))
  P <- dim(object$freqCalc)[1]
  cat("\n Suppressions: \n")
  for (i in 1:length(a1)) {
    if (a1[i] != 0)
      cat(paste("\nLocal suppression in", names(a1)[i], ":", a1[i], "/", P, "\n"))
  }
}
