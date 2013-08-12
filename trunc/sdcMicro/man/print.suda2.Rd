\name{print.suda2}
\alias{print.suda2}
\title{ Print method for objects from class suda2 }
\description{
Print method for objects from class suda2.
}
\usage{
\method{print}{suda2}(x, ...)
}
\arguments{
  \item{x}{ an object of class suda2 }
  \item{\dots}{ additional arguments passed through. }
}
\value{
Table of dis suda scores. 
}
\author{ Matthias Templ }
\seealso{ \code{\link{suda2}} }
\examples{
data(testdata)
data_suda2 <- suda2(testdata,variables=c("urbrur","roof","walls","water","sex"))
data_suda2
}
\keyword{ print }
