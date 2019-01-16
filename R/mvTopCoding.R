#' @name mvTopCoding
#' @title Detection and winsorization of multivariate outliers
#' @rdname mvTopCoding
#' @usage mvTopCoding(x, maha=NULL,center=NULL,cov=NULL, alpha=0.025)
#' @author Johannes Gussenbauer, Matthias Templ
#' @description Imputation and detection of outliers 
#' @details Winsorizes the potential outliers on the ellipsoid defined by MD2 in direction to the center of the data
#' @param x object of class matrix with numeric entries
#' @param maha squared mahalanobis distance of each observation
#' @param center center of data, needed for calcualtion of mahalanobis distance (if not provide)
#' @param cov covariance matrix of data, needed for calcualtion of mahalanobis distance (if not provide)
#' @param alpha significance level, determining the ellipsoide to which outliers should be placed upon
#' @return the imputed winsorized data
#' @importFrom robustbase covMcd
#' @importFrom MASS mvrnorm
#' @export 
#' @examples 
#' set.seed(123)
#' x <- MASS::mvrnorm(20, mu = c(5,5), Sigma = matrix(c(1,0.9,0.9,1), ncol = 2))
#' x[1,1] <- 3
#' x[1,2] <- 6
#' plot(x)
#' ximp <- mvTopCoding(x)
#' 
#' # by hand (non-robust)
#' x[2,2] <- NA
#' m <- colMeans(x, na.rm = TRUE)
#' s <- cov(x, use = "complete.obs")
#' md <- mahalanobis(x, m, s)
#' ximp <- mvTopCoding(x, center = m, cov = s, maha = md)
#' 
mvTopCoding <- function(x, maha=NULL,center=NULL,
                           cov=NULL, alpha=0.025){
  stopifnot(is.numeric(x) || is.logical(x), is.atomic(x))
 
  if(!is.data.table(x))x <- as.data.table(x)
  
  p <- ncol(x)
  d <- qchisq(1-alpha,df=p)
  
  if(is.null(maha)){
    cv <- robustbase::covMcd(x=x)
    center <- cv$center
    maha <- mahalanobis(x=x, center=center,cov=cv$cov)
  }
  
  index <- which(maha > d)
  a <- as.matrix(sqrt(d/maha[index]))
  
  x[index] <- x[index]*a+(1-a)%*%t(unlist(center))
  
  return(x)
}