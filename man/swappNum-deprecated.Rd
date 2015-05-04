\name{swappNum-deprecated}
\alias{swappNum-deprecated}
\title{ Rank Swapping }
\description{
Rank Swapping.
}
\usage{
swappNum(x, w = 1:(dim(x)[2]), p)
}
\arguments{
  \item{x}{ matrix or data frame }
  \item{w}{ variables, on which rank swapping should be applied }
  \item{p}{ Percentage. Swapping range. }
}
\details{
The values of a variable are
ranked, then each ranked value is swapped with
another ranked value randomly chosen within a restricted range, i.e. the rank
of two swapped values cannot differ by more than p percente of the total number of
records. The function apply the rank swapping on each variable independently.
}
\value{
  \item{x }{original data}
  \item{xm  }{the rank swapped data }
  \item{method  }{ info about the method name }
}
\author{ Matthias Templ}
\seealso{ \code{\link{microaggregation}} }
\examples{
## Numerical Rank Swapping:
data(free1)
free1[, 31:34] <- rankSwap(free1[, 31:34], P=10) 
}
\keyword{ manip }

