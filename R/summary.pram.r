`summary.pram` <-
function(object, ...){
  rr <- apply(rbind(object$x,object$xpramed), 2, paste, collapse=" --> ")
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

