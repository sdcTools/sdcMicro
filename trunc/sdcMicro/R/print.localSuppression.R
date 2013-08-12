print.localSuppression <- function(x, ...){
  cat("\n -----------------------\n")
  print(paste("Total Suppressions in the key variables ", x$totalSupps, sep=""))
  print(paste("Number of suppressions in the key variables "))
  cat("\n", x$supps, "\n ------------\n")
  print(paste(x$k,"-anonymity == ", x$anonymity, sep=""))
  cat("\n -----------------------\n")
}