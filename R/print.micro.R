#' Print method for objects from class micro
#'
#' Print method for objects from class micro.
#'
#'
#' @param x object from class micro
#' @param \dots Additional arguments passed through.
#' @return information about method and aggregation level from objects of class
#' micro.
#' @author Matthias Templ
#' @seealso \code{\link{microaggregation}}
#' @keywords print
#' @method print micro
#' @export
#' @examples
#'
#' data(free1)
#' m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
#' m1
#'
print.micro <- function(x, ...) {
  cat(paste("\n Object created with method", x$method, "and aggregation level", x$aggr))
  cat("\n -------------------------\n")
  cat("x ... original values \n")
  print(summary(x$x))
  cat("\n -------------------------\n")
  cat("mx ... microaggregated values\n")
  print(summary(x$mx))
  cat("\n -------------------------\n")

  cat("Try names(your object from class micro) for more details")
  cat("\n")
}
