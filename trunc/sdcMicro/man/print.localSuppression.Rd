\name{print.localSuppression}
\alias{print.localSuppression}
\title{ Print method for objects from class localSuppression }
\description{
Print method for objects from class localSuppression.
}
\usage{
\method{print}{localSuppression}(x, ...)
}
\arguments{
  \item{x}{ object from class localSuppression }
  \item{\dots}{ Additional arguments passed through. }
}
\value{
Information about the frequency counts for key variables for object of class \sQuote{localSuppression}.
}
\author{ Matthias Templ }
\seealso{ \code{\link{localSuppression}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
l1 <- localSuppression(francdat, keyVars=c(2,4,5,6))
l1
}
\keyword{ print }

