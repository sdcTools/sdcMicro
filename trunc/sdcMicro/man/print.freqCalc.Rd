\name{print.freqCalc}
\alias{print.freqCalc}
\title{ Print method for objects from class freqCalc }
\description{
Print method for objects from class freqCalc.
}
\usage{
\method{print}{freqCalc}(x, ...)
}
\arguments{
  \item{x}{ object from class freqCalc }
  \item{\dots}{ Additional arguments passed through. }
}
\value{
information about the frequency counts for key variables for object of class \sQuote{freqCalc}.
}
\author{ Matthias Templ }
\seealso{ \code{\link{freqCalc}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
f
}
\keyword{ print }

