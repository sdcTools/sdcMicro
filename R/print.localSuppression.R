#' Print method for objects from class localSuppression
#'
#' Print method for objects from class localSuppression.
#'
#'
#' @param x object from class localSuppression
#' @param \dots Additional arguments passed through.
#' @return Information about the frequency counts for key variables for object
#' of class \sQuote{localSuppression}.
#' @author Matthias Templ
#' @seealso \code{\link{localSuppression}}
#' @keywords print
#' @method print localSuppression
#' @export
#' @examples
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' l1 <- localSuppression(francdat, keyVars=c(2,4,5,6))
#' l1
#'
print.localSuppression <- function(x, ...) {
  cat("\n -----------------------\n")
  print(paste("Total Suppressions in the key variables ", x$totalSupps, sep = ""))
  print(paste("Number of suppressions in the key variables "))
  cat("\n", x$supps, "\n ------------\n")
  print(paste(x$k, "-anonymity == ", x$anonymity, sep = ""))
  cat("\n -----------------------\n")
}
