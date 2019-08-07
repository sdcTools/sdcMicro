#' Comparison of different microaggregation methods
#'
#' A Function for the comparison of different perturbation methods.
#'
#' Tabularize the output from [summary.micro()]. Will be enhanced to all
#' perturbation methods in future versions.
#'
#' @param x a `data.frame` or a `matrix`
#' @param method character vector defining names of microaggregation-, adding-noise
#' or rank swapping methods.
#' @param measure FUN for aggregation. Possible values are mean (default), median, trim, onestep.
#' @param clustermethod clustermethod, if a method will need a clustering procedure
#' @param aggr aggregation level (default=3)
#' @param nc number of clusters. Necessary, if a method will need a clustering procedure
#' @param transf Transformation of variables before clustering.
#' @param p Swapping range, if method swappNum has been chosen
#' @param noise noise addition, if an addNoise method has been chosen
#' @param w variables for swapping, if method swappNum has been chosen
#' @param delta parameter for adding noise method `"correlated2"`
#' @return Measures of information loss splitted for the comparison of different methods.
#'
#' @details Methods for adding noise should be named via `addNoise:{method}`, e.g.
#' `addNoise:correlated`, where `{method}` specifies the desired method as
#' described in [addNoise()].
#' @author Matthias Templ
#' @seealso [microaggregation()], [summary.micro()]
#' @references Templ, M. and Meindl, B., `Software Development for SDC in
#' R`, Lecture Notes in Computer Science, Privacy in Statistical Databases,
#' vol. 4302, pp. 347-359, 2006.
#' @keywords print
#' @export
#' @md
#' @examples
#' data(Tarragona)
#' \dontrun{
#' valTable(
#'   x = Tarragona[100:200, ],
#'   method=c("simple", "onedims", "pca"))
#'
#' valTable(
#'   x = Tarragona,
#'   method = c("simple", "onedims", "pca", "clustpppca", "mdav", "swappNum"))
#'
#' ## clustpppca in combination with Mclust outperforms
#' ## the other algorithms for this data set...
#' }
valTable <- function(x, method = c("simple", "onedims", "clustpppca", "addNoise: additive", "swappNum"),
  measure = "mean", clustermethod = "clara", aggr = 3, nc = 8, transf = "log",
  p = 15, noise = 15, w = 1:dim(x)[2], delta = 0.1) {

  # vars to keep from summary.micro()
  keep <- c("amean", "amedian", "aonestep", "devvar", "amad", "acov", "acor",
            "acors", "adlm", "apcaload", "apppcaload", "atotals", "pmtotals",
            "util1", "deigenvalues", "risk0", "risk1", "risk2", "wrisk1", "wrisk2")

  m <- out <- vector("list", length(method))
  for (i in 1:length(method)) {
    message("method ", i, "|", length(method), ": ", shQuote(method[i]))
    message("--> compute results")
    if (method[i] %in% c("simple", "single", "onedims", "pca", "pppca", "clustpca",
                         "clustpppca", "mdav", "influence", "rmd", "clustmcdpca", "mcdpca")) {
      m[[i]] <- microaggregation(
          obj = x,
          method = method[i],
          measure = measure,
          clustermethod = clustermethod,
          aggr = aggr,
          nc = nc,
          transf = transf)
    }
    if (method[i] == "swappNum") {
      m[[i]] <- list(x = x, mx = rankSwap(x, P = p, R0 = NULL))
    }
    if (substring(method[i], 1, 8) == "addNoise") {
      adn_meth <-  strsplit(method[i], ":")[[1]][2]
      if (is.na(adn_meth)) {
        stop(shQuote(method[i]), "is not a valid in argument `method`.", call. = FALSE)
      }
      m[[i]] <- list(x = x, mx = addNoise(obj = x, noise = noise, method = adn_meth)$xm)
    }
    if (method[i] == "dataGen") {
      m[[i]] <- list(x = x, xm = dataGen(x, n = dim(x)[1]))
    }
    message("--> compute summary statistics")
    out[[i]] <- as.data.frame(summary.micro(m[[i]])[keep])
  }
  g <- do.call("rbind", out)
  g <- apply(g, 2, function(x) round(x, digits = 3))
  g <- cbind(data.frame(method = method), g)
  g
}
