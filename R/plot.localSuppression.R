#' plot method for localSuppression objects
#'
#' Barplot for objects from class localSuppression.
#'
#' Just look at the resulting plot.
#'
#' @param x object of class \sQuote{localSuppression}
#' @param \dots Additional arguments passed through.
#' @author Matthias Templ
#' @seealso \code{\link{localSuppression}}
#' @keywords aplot
#' @method plot localSuppression
#' @export
#' @examples
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' l1 <- localSuppression(francdat, keyVars=c(2,4,5,6))
#' l1
#' plot(l1)
#'
plot.localSuppression <- function(x, ...) {
  checkCN <- function(x) {
    if (max(sapply(x, nchar, type = "width")) > 11) {
      warning("Too long variable names are cutted")
    }
    w <- which(sapply(x, nchar, type = "width") > 11)
    x[w] <- substr(x[w], 1, 11)
    x
  }
  b <- barplot(x$supps, names.arg = checkCN(names(x$supps)), col = "red", ylab = "Number of Suppressions",
    xlab = "key variables")
  mtext(side = 1, at = b, line = -2, text = paste(formatC(x$supps)), col = "black")
}
