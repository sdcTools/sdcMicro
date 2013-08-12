\name{summary.pram}
\alias{summary.pram}
\title{ Summary method for objects from class pram }
\description{
Summary method for objects from class \sQuote{pram} to provide information about transitions.
}
\usage{
\method{summary}{pram}(object, ...)
}
\arguments{
  \item{object}{object from class \sQuote{pram} }
  \item{\dots}{ Additional arguments passed through.  }
}
\details{
  Shows various information about the transitions.
}
\value{
  The summary of object from class \sQuote{pram}.
}
\references{
Templ, M. 
               \emph{Statistical Disclosure Control for Microdata Using the R-Package sdcMicro}, 
               Transactions on Data Privacy, 
               vol. 1, number 2, pp. 67-85, 2008. 
  \url{http://www.tdp.cat/issues/abs.a004a08.php}
}
\author{Matthias Templ }
\seealso{ \code{\link{pram}} }
\examples{
data(free1)
x <- free1[,"MARSTAT"]
x2 <- pram(x)
x2
summary(x2)
}
\keyword{ print }

