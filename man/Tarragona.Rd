\name{Tarragona}
\alias{Tarragona}
\docType{data}
\title{ Tarragona data set }
\description{
A real data set comprising figures of 834 companies in the Tarragona area. Data correspond to year 1995.
}
\usage{data(Tarragona)}
\format{
  A data frame with 834 observations on the following 13 variables.
  \describe{
    \item{\code{FIXED.ASSETS}}{a numeric vector}
    \item{\code{CURRENT.ASSETS}}{a numeric vector}
    \item{\code{TREASURY}}{a numeric vector}
    \item{\code{UNCOMMITTED.FUNDS}}{a numeric vector}
    \item{\code{PAID.UP.CAPITAL}}{a numeric vector}
    \item{\code{SHORT.TERM.DEBT}}{a numeric vector}
    \item{\code{SALES}}{a numeric vector}
    \item{\code{LABOR.COSTS}}{a numeric vector}
    \item{\code{DEPRECIATION}}{a numeric vector}
    \item{\code{OPERATING.PROFIT}}{a numeric vector}
    \item{\code{FINANCIAL.OUTCOME}}{a numeric vector}
    \item{\code{GROSS.PROFIT}}{a numeric vector}
    \item{\code{NET.PROFIT}}{a numeric vector}
  }
}
\source{
  Public use data from the CASC project. 
}
\references{
Brand, R. and
Domingo-Ferrer, J. and Mateo-Sanz, J.M., Reference data sets to test and compare SDC methods for protection of numerical microdata.
Unpublished. \url{http://neon.vb.cbs.nl/casc/CASCrefmicrodata.pdf}
}
\examples{
data(Tarragona)
head(Tarragona)
dim(Tarragona)
}
\keyword{datasets}
