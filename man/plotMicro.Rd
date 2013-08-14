\name{plotMicro}
\alias{plotMicro}
\title{ Comparison plots }
\description{
  Plots for the comparison of the original data and perturbed data.
}
\usage{
plotMicro(x, p, which.plot = 1:3)
}
\arguments{
  \item{x}{ object from class micro }
  \item{p}{ necessary parameter for the box cox transformation (lambda) }
  \item{which.plot}{ which plot should be created? 1: density traces, 2: parallel boxplots, 3: differences in totals }
}
\details{
Univariate and multivariate comparison plots are implemented to 
detect differences between the perturbed and the original data, but also 
to compare perturbed data which are produced by different methods.
}
\references{ 
  Templ, M. and Meindl, B., 
               \emph{Software Development for SDC in R}, 
               Lecture Notes in Computer Science, Privacy in Statistical Databases, 
               vol. 4302, pp. 347-359, 2006.   
}
\author{ Matthias Templ }
\seealso{  \code{\link{microaggregation}} }
\examples{
data(free1)
m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
m2 <- microaggregation(free1[, 31:34], method="pca", aggr=3)
plotMicro(m1, 0.1, which.plot=1)
}
\keyword{ aplot }
