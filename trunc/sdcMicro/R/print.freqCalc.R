`print.freqCalc` <-
function(x, ...){
  P <- dim(x)[2]
  cat("\n --------------------------\n")
  cat(paste(x$n1, "obs. violate 2-anonymity \n"))
  cat(paste(x$n2+x$n1, "obs. violate 3-anonymity \n"))
  cat(" --------------------------\n")  
}

