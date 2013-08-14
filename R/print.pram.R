`print.pram` <-
function(x, ...){
  P <- dim(x)[2]
  cat("\n this vector is perturbed with")
  cat("\n invariant PRAM \n")
  cat("\n Parameters for PRAM: ")
  cat(paste("\n alpha = ", x$alpha))
  cat(paste("\n minimum diagonal element = ", x$pd, "\n"))
}

