\name{plot.localSuppression}
\alias{plot.localSuppression}
\title{ plot method for localSuppression objects }
\description{
 Barplot for objects from class localSuppression.
}
\usage{
\method{plot}{localSuppression}(x, ...)
}
\arguments{
  \item{x}{ object of class \sQuote{localSuppression} }
  \item{\dots}{ Additional arguments passed through. }
}
\details{
  Just look at the resulting plot.
}
\author{ Matthias Templ }
\seealso{ \code{\link{localSuppression}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
l1 <- localSuppression(francdat, keyVars=c(2,4,5,6))
l1
plot(l1)
}
\keyword{ aplot }
