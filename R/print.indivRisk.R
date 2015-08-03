#' Print method for objects from class indivRisk
#'
#' Print method for objects from class indivRisk
#'
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
