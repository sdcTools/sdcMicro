plot.localSuppression <- function(x, ...){
  checkCN <- function(x) {
        if (max(sapply(x, nchar)) > 11) {
            warning("Too long variable names are cutted")
        }
        w <- which(sapply(x, nchar) > 11)
        x[w] <- substr(x[w], 1, 11)
        x
    }
  b <- barplot(x$supps, names.arg = checkCN(names(x$supps)), col="red",
          ylab="Number of Suppressions", xlab="key variables")
  mtext(side = 1, at = b, line = -2,
        text = paste(formatC(x$supps)), col = "black")

}