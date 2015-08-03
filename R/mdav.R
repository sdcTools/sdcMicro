# SEXP g_MissingValue_R,SEXP weights_R,SEXP g_K_R
mdav <- function(data, variables = NULL, weights = NULL, K = 10, missing = -999) {
  if (is.null(variables)) {
    variables <- colnames(data)
  }
  if (is.null(weights)) {
    weights <- rep(1, length(variables))
  } else {
    if (length(weights) != length(variables)) {
      stop("There must be a weight for each variable")
    }
  }

  dataX <- data[, variables]
  dataX <- as.matrix(dataX)
  for (i in 1:ncol(dataX)) {
    if (!is.numeric(dataX[, i])) {
      dataX[, i] <- as.numeric(dataX[, i])
    }
  }
  data2 <- dataX
  TF <- is.na(dataX)
  dataX[TF] <- missing
  data2[, ] <- NA
  dat <- .Call("Mdav", dataX, data2, missing, weights, K)$Res
  try(dat[TF] <- NA)
  data[, variables] <- dat
  invisible(data)
}
