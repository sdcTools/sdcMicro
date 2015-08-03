#' Print method for objects from class freqCalc
#'
#' Print method for objects from class freqCalc.
#'
#'
#' @param x object from class \code{\link{freqCalc}}
#' @param \dots Additional arguments passed through.
#' @return information about the frequency counts for key variables for object
#' of class \code{\link{freqCalc}}.
#' @author Matthias Templ
#' @seealso \code{\link{freqCalc}}
#' @keywords print
#' @method print freqCalc
#' @export
#' @examples
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
#' f
#'
print.freqCalc <- function(x, ...) {
  P <- dim(x)[2]
  cat("\n --------------------------\n")
  cat(paste(x$n1, "obs. violate 2-anonymity \n"))
  cat(paste(x$n2 + x$n1, "obs. violate 3-anonymity \n"))
  cat(" --------------------------\n")
}
