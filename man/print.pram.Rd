\name{print.pram}
\alias{print.pram}
\title{ Print method for objects from class pram }
\description{
Print method for objects from class \sQuote{pram}.
}
\usage{
\method{print}{pram}(x, ...)
}
\arguments{
  \item{x}{ object from class \sQuote{pram} }
  \item{\dots}{ Additional arguments passed through.  }
}
\value{
Short information about the method and the parameters used.
}
\references{
Templ, M. 
               \emph{Statistical Disclosure Control for Microdata Using the R-Package sdcMicro}, 
               Transactions on Data Privacy, 
               vol. 1, number 2, pp. 67-85, 2008. 
  \url{http://www.tdp.cat/issues/abs.a004a08.php}
}
\author{ Matthias Templ }
\seealso{ \code{\link{pram}} }
\examples{
data(free1)
x <- free1[,"MARSTAT"]
x2 <- pram(x)
x2
}
\keyword{ print }

