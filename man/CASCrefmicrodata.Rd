\name{CASCrefmicrodata}
\alias{CASCrefmicrodata}
\docType{data}
\title{ Census data set }
\description{
This test data set was obtained on July 27, 2000 using the public use Data Extraction System 
of the U.S. Bureau of the Census.
}
\usage{data(CASCrefmicrodata)}
\format{
  A data frame sampled from year 1995 with 1080 observations on the following 13 variables.
  \describe{
    \item{\code{AFNLWGT}}{Final weight (2 implied decimal places)}
    \item{\code{AGI}}{Adjusted gross income}
    \item{\code{EMCONTRB}}{Employer contribution for hlth insurance}
    \item{\code{FEDTAX}}{Federal income tax liability}
    \item{\code{PTOTVAL}}{Total person income}
    \item{\code{STATETAX}}{State income tax liability}
    \item{\code{TAXINC}}{Taxable income amount}
    \item{\code{POTHVAL}}{Total other persons income}
    \item{\code{INTVAL}}{Amt of interest income}
    \item{\code{PEARNVAL}}{Total person earnings}
    \item{\code{FICA}}{Soc. sec. retirement payroll deduction}
    \item{\code{WSALVAL}}{Amount: Total Wage and salary}
    \item{\code{ERNVAL}}{Business or Farm net earnings}
  }
}
\source{
  Public use file from the CASC project. 
  More information on this test data can be found in the paper listed below.
}
\references{
Brand, R. and
Domingo-Ferrer, J. and Mateo-Sanz, J.M., Reference data sets to test and compare SDC methods for protection of numerical microdata. 
Unpublished. \url{http://neon.vb.cbs.nl/casc/CASCrefmicrodata.pdf}
}
\examples{
data(CASCrefmicrodata)
str(CASCrefmicrodata)
}
\keyword{datasets}
