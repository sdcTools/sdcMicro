\name{plot.indivRisk}
\alias{plot.indivRisk}
\title{ plot method for indivRisk objects }
\description{
 Plots an interactive histogramm or ecdf plot with various interactive sliders.
}
\usage{
\method{plot}{indivRisk}(x, ...)
}
\arguments{
  \item{x}{ object of class \sQuote{indivRisk} }
  \item{\dots}{ Additional arguments passed through. }
}
\details{
  With the sliders one can move the individual risk threshold. By this movement the threshold 
  will be moved on the plot and the slider with a re-idendification rate and the slider of the number of unsafe records (based on your
  chosen threshold) are also moved based on the individual risk threshold. This plot is very similar to the individual risk plot of
  the software mu-Argus.
}
\references{ look e.g. on the mu-Argus manuals available at \url{http://neon.vb.cbs.nl/casc/Software/MuManual4.1.pdf}

Templ, M.  
               \emph{Statistical Disclosure Control for Microdata Using the R-Package sdcMicro}, 
               Transactions on Data Privacy, 
               vol. 1, number 2, pp. 67-85, 2008. 
  \url{http://www.tdp.cat/issues/abs.a004a08.php}
}
\author{ Matthias Templ }
\seealso{ \code{\link{indivRisk}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
ff <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
irisk <- indivRisk( ff ) 
## and now apply:
## plot(irisk)
data(free1)
ff <- freqCalc(free1, keyVars=1:3, w=30)
irisk2 <- indivRisk(ff)
## and now apply:
## plot(irisk2) 
}
\keyword{ aplot }
