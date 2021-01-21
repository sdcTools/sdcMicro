#' argus_rankswap
#'
#' @param df a \code{data.frame} with only numerical columns
#' @param perc a number defining the swapping percantage
#'
#' @return a \code{list} with two elements
#' \itemize{
#' \item{original: } the originally provided input data
#' \item{swapped: } the \code{data.frame} containing the swapped values
#' }
#' @seealso mu-Argus manual at \url{https://github.com/sdcTools/manuals/raw/master/mu-argus/MUmanual5.1.pdf}
#' @export
#' @examples
#' mat <- matrix(sample(1:100, 50, replace=TRUE), nrow=10, ncol=5)
#' df <- as.data.frame(mat)
#' res <- argus_rankswap(df, perc=10)
argus_rankswap <- function(df, perc) {
  stopifnot("data.frame" %in% class(df))
  if ("data.table" %in% class(df)) {
    class(df) <- "data.frame" # no data.tables
  }
  stopifnot(all(sapply(df, is.numeric)))
  df_o <- df
  inp <- as.matrix(df)
  stopifnot(is.numeric(inp))
  stopifnot(length(perc)==1)
  stopifnot(all.equal(perc, as.integer(perc)))
  stopifnot(perc > 0)
  stopifnot(perc <= 100)
  perc <- as.integer(perc)

  # call the .C++-function exported by Rcpp
  res <- rankSwap_argus_cpp(inp=inp, perc=perc)

  # check for return-code and return result
  if (res$ret_code!=1) {
    stop("Procedure stopped. Mu-Argus Errorcode:", res$ret_code)
  }

  swapped <- as.data.frame(res$inp)
  colnames(swapped) <- colnames(df_o)
  return(list(original=df_o, swapped=swapped))
}


#' argus_microaggregation
#'
#' calls microaggregation code from mu-argus. In case only one variable should be
#' microaggregated and \code{useOptimal} is \code{TRUE}, Hansen-Mukherjee polynomial exact method
#' is applied. In any other case, the Mateo-Domingo method is used.
#'
#' @param df a \code{data.frame} with only numerical columns
#' @param k required group size
#' @param useOptimal (logical) should optimal microaggregation be applied (ony possible in
#' in case of one variable)
#'
#' @return a \code{list} with two elements
#' \itemize{
#' \item{original: } the originally provided input data
#' \item{microaggregated: } the microaggregated data.frame
#' }
#' @seealso mu-Argus manual at \url{https://github.com/sdcTools/manuals/raw/master/mu-argus/MUmanual5.1.pdf}
#' @export
#'
#' @examples
#' mat <- matrix(sample(1:100, 50, replace=TRUE), nrow=10, ncol=5)
#' df <- as.data.frame(mat)
#' res <- argus_microaggregation(df, k=5, useOptimal=FALSE)
argus_microaggregation <- function(df, k, useOptimal=FALSE) {
  stopifnot("data.frame" %in% class(df))
  if ("data.table" %in% class(df)) {
    class(df) <- "data.frame" # no data.tables
  }
  stopifnot(all(sapply(df, is.numeric)))
  df_o <- df

  stopifnot(length(k)==1)
  stopifnot(all.equal(k, as.integer(k)))
  k <- as.integer(k)
  stopifnot(k>1)

  stopifnot(length(useOptimal)==1)
  stopifnot(is.logical(useOptimal))

  # call the .C++-function exported by Rcpp
  res <- microaggregation_argus_cpp(inp=as.matrix(df), k=k, useOptimal=as.numeric(useOptimal))

  # check for return-code and return result
  if (res$ret_code!=1) {
    stop("Procedure stopped. Mu-Argus Errorcode:", res$ret_code)
  }

  microaggregated <- as.data.frame(res$inp)
  colnames(microaggregated) <- colnames(df_o)

  return(list(original=df_o, microaggregated=microaggregated))
}
