`print.micro` <-
function(x, ...){
  cat(paste("\n Object created with method", x$method,"and aggregation level",x$aggr))
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

