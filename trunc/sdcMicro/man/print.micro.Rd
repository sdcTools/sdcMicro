\name{print.micro}
\alias{print.micro}
\title{ Print method for objects from class micro }
\description{
Print method for objects from class micro.
}
\usage{
\method{print}{micro}(x, ...)
}
\arguments{
  \item{x}{ object from class micro }
  \item{\dots}{ Additional arguments passed through.  }
}
\value{
information about method and aggregation level from objects of class micro.
}
\author{ Matthias Templ }
\seealso{ \code{\link{microaggregation}} }
\examples{
data(free1)
m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
m1
}
\keyword{ print }

