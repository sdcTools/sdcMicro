#' Dummy Dataset for Record Swapping
#'
#' [createDat()] returns dummy data to illustrate
#' targeted record swapping. The generated data contain
#' household ids (`hid`), geographic variables
#' (`nuts1`, `nuts2`, `nuts3`, `lau2`) as well as some
#' other household or personal variables.
#'
#' @param N integer, number of household to generate
#' @seealso recordSwap
#'
#' @return `data.table` containing dummy data
#' @rdname recordSwap
#' @export
createDat <- function(N = 10000) {
  stopifnot(is.numeric(N))
  stopifnot(N > 1)
  N <- ceiling(N)
  nuts1 <- sample(1:3, N, replace = TRUE)
  nuts2 <- sample(1:5, N, replace = TRUE)
  nuts3 <- sample(1:15, N, replace = TRUE)
  lau2 <- sample(1:5, N, replace = TRUE)
  hsize <- sample(1:6, N, replace = TRUE)
  htype <- sample(1:10, N, replace = TRUE)
  hincome <- sample(1:10, N, replace = TRUE)
  dat <- data.table(
    nuts1 = rep(nuts1, times = hsize),
    nuts2 = rep(nuts2, times = hsize),
    nuts3 = rep(nuts3, times = hsize),
    lau2 = rep(lau2, times = hsize),
    hid = rep(1:length(hsize), times = hsize),
    hsize = rep(hsize, times = hsize),
    ageGroup = sample(1:7, length(hsize), replace = TRUE),
    gender = sample(c(1, 2), length(hsize), replace = TRUE),
    national = sample(1:5, length(hsize), replace = TRUE),
    htype = rep(htype, times = hsize),
    hincome = rep(hincome, times = hsize)
  )

  # hierarchy for regional variables
  help_0 <- c("", "0", "00", "000")
  dat[, nuts2 := paste0(nuts1, nuts2)]
  dat[, nuts3 := paste0(nuts2, help_0[3 - nchar(nuts3)], nuts3)]
  dat[, lau2 := paste0(nuts3, help_0[5 - nchar(nuts3)], lau2)]
  dat[, colnames(dat) := lapply(.SD, as.integer)]
  return(dat)
}
