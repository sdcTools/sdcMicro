\name{francdat}
\alias{francdat}
\docType{data}
\title{ data from the casc project}
\description{
 Small synthetic data from Capobianchi, Polettini, Lucarelli
}
\usage{data(francdat)}
\format{
  A data frame with 8 observations on the following 8 variables.
  \describe{
    \item{\code{Num1}}{a numeric vector}
    \item{\code{Key1}}{Key variable 1. A numeric vector}
    \item{\code{Num2}}{a numeric vector}
    \item{\code{Key2}}{Key variable 2. A numeric vector}
    \item{\code{Key3}}{Key variable 3. A numeric vector}
    \item{\code{Key4}}{Key variable 4. A numeric vector}
    \item{\code{Num3}}{a numeric vector}
    \item{\code{w}}{The weight vector. A numeric vector}
  }
}
\details{
This data set is very similar to that one which are used by the authors of the 
paper given below. We need this data set only for demonstration effect, i.e. that the 
package provides the same results as their software.
}
\source{
\url{http://neon.vb.cbs.nl/casc/Deliv/12d1.pdf}
}
\examples{
data(francdat)
francdat
}
\keyword{datasets}
