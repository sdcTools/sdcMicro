#' Summary method for objects from class pram
#'
#' Summary method for objects from class \sQuote{pram} to provide information
#' about transitions.
#'
#' Shows various information about the transitions.
#'
#' @param object object from class \sQuote{pram}
#' @param \dots Additional arguments passed through.
#' @return The summary of object from class \sQuote{pram}.
#' @author Matthias Templ
#' @seealso \code{\link{pram}}
#' @references Templ, M.  \emph{Statistical Disclosure Control for Microdata
#' Using the R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number
#' 2, pp. 67-85, 2008.  \url{http://www.tdp.cat/issues/abs.a004a08.php}
#' @keywords print
#' @export
#' @examples
#'
#' data(free1)
#' x <- free1[,"MARSTAT"]
#' x2 <- pram(x)
#' x2
#' summary(x2)
#'
summary.pram <- function(object, ...) {
  rr <- apply(rbind(object$x, object$xpramed), 2, paste, collapse = " --> ")
  result <- data.frame(table(rr))
  rownames(result) <- 1:nrow(result)
  colnames(result) <- c("transition", "Frequency")
  cat("\n ----------------------")
  cat("\n original frequencies:\n")
  print(table(object$x))
  cat("\n ----------------------")
  cat("\n frequencies after perturbation:\n")
  print(table(object$x_pram))  #
  cat("\n ----------------------")
  cat("\n transitions:\n")
  result
}
