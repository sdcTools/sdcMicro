#' @name mvTopCoding
#' @title Detection and winsorization of multivariate outliers
#' @rdname mvTopCoding
#' @usage mvTopCoding(x, maha=NULL,center=NULL,cov=NULL, alpha=0.025)
#' @author Johannes Gussenbauer, Matthias Templ
#' @description Imputation and detection of outliers
#' @details Winsorizes the potential outliers on the ellipsoid defined by (robust) Mahalanobis distances in direction to the center of the data
#' @param x object of class matrix with numeric entries
#' @param maha squared mahalanobis distance of each observation
#' @param center center of data, needed for calcualtion of mahalanobis distance (if not provide)
#' @param cov covariance matrix of data, needed for calcualtion of mahalanobis distance (if not provide)
#' @param alpha significance level, determining the ellipsoide to which outliers should be placed upon
#' @return the imputed winsorized data
#' @importFrom robustbase covMcd
#' @importFrom MASS mvrnorm
#' @importFrom data.table is.data.table
#' @export
#' @examples
#' set.seed(123)
#' x <- MASS::mvrnorm(20, mu = c(5,5), Sigma = matrix(c(1,0.9,0.9,1), ncol = 2))
#' x[1,1] <- 3
#' x[1,2] <- 6
#' plot(x)
#' ximp <- mvTopCoding(x)
#' points(ximp, col = "blue", pch = 4)
#'
#' # more dimensions
#' Sigma <- diag(5)
#' Sigma[upper.tri(Sigma)] <- 0.9
#' Sigma[lower.tri(Sigma)] <- 0.9
#' x <- MASS::mvrnorm(20, mu = rep(5,5), Sigma = Sigma)
#' x[1,1] <- 3
#' x[1,2] <- 6
#' par(mfrow = c(1,2))
#' pairs(x)
#' ximp <- mvTopCoding(x)
#' xnew <- data.frame(rbind(x, ximp))
#' xnew$beforeafter <- rep(c(0,1), each = nrow(x))
#'
#' pairs(xnew, col = xnew$beforeafter, pch = 4)
#'
#' # by hand (non-robust)
#' x[2,2] <- NA
#' m <- colMeans(x, na.rm = TRUE)
#' s <- cov(x, use = "complete.obs")
#' md <- stats::mahalanobis(x, m, s)
#' ximp <- mvTopCoding(x, center = m, cov = s, maha = md)
#' plot(x)
#' points(ximp, col = "blue", pch = 4)
#'
mvTopCoding <- function(x, maha=NULL,center=NULL,
                        cov=NULL, alpha=0.025){
  # stopifnot(is.numeric(x) || is.logical(x), is.atomic(x))

  # if(is.data.frame(x) | is.data.table(x)){
  #   stop("x must be of class matrix")
  # }
  if(!is.data.table(x)) x <- as.data.table(x)
  p <- ncol(x)
  d <- stats::qchisq(1-alpha,df=p)

  if(is.null(maha)){
    cv <- robustbase::covMcd(x=x)
    center <- cv$center
    maha <- stats::mahalanobis(x=x, center=center,cov=cv$cov)
  }

  index <- which(maha > d)
  a <- as.matrix(sqrt(d/maha[index]))

  x[index] <- x[index]*a+(1-a)%*%t(unlist(center))

  return(x)
}
