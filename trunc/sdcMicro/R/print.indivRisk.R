`print.indivRisk` <-
function(x, ...){
#  cat("\n ----- individual risk ----- \n")
  cat(paste("method=", x$method, ", qual=", x$qual, sep=""))
  cat("\n --------------------------- \n")
  s <- sum(x$rk > median(x$rk)+3*mad(x$rk) & x$rk > 0.1)
  cat(paste(s,"obs. with high risk"))
}

