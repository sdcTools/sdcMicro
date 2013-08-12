\name{summary.micro}
\alias{summary.micro}
\title{ Summary method for objects from class micro }
\description{
Summary method for objects from class \sQuote{micro}.
}
\usage{
\method{summary}{micro}(object, ...)
}
\arguments{
  \item{object}{ objects from class micro }
  \item{\dots}{ Additional arguments passed through.  }
}
\details{
This function computes several measures of information loss, such as 
}
\value{
  \item{meanx }{A conventional summary of the original data}
  \item{meanxm }{A conventional summary of the microaggregated data}
  \item{amean }{average relative absolute deviation of means }
  \item{amedian }{average relative absolute deviation of medians }
  \item{aonestep }{average relative absolute deviation of onestep from median}
  \item{devvar }{average relative absolute deviation of variances }
  \item{amad }{average relative absolute deviation of the mad}
  \item{acov }{average relative absolute deviation of covariances }
  \item{arcov }{average relative absolute deviation of robust (with mcd) covariances }
  \item{acor }{average relative absolute deviation of correlations }
  \item{arcor }{average relative absolute deviation of robust (with mcd) correlations }
  \item{acors  }{average relative absolute deviation of rank-correlations  }
  \item{adlm  }{average absolute deviation of lm regression coefficients (without intersept)  }
  \item{adlts  }{average absolute deviation of lts regression coefficients (without intersept)  }
  \item{apcaload  }{average absolute deviation of pca loadings  }
  \item{apppacaload  }{average absolute deviation of robust (with projection pursuit approach) pca loadings  }
   \item{atotals  }{average relative absolute deviation of totals  }
  \item{pmtotals  }{average relative deviation of totals  }
   
}
\references{
Templ, M. 
               \emph{Statistical Disclosure Control for Microdata Using the R-Package sdcMicro}, 
               Transactions on Data Privacy, 
               vol. 1, number 2, pp. 67-85, 2008. 
  \url{http://www.tdp.cat/issues/abs.a004a08.php}
}
\author{ Matthias Templ }
\seealso{\code{\link{microaggregation}}, \code{\link{valTable}}}
\examples{
data(Tarragona)
m1 <- microaggregation(Tarragona, method="onedims", aggr=3)
## summary(m1)
}
\keyword{ print }

