\name{indivRisk}
\alias{indivRisk}
\title{ Individual Risk computation }
\description{
Individual risk computation.
}
\usage{
indivRisk(x, method = "approx", qual = 1, survey=TRUE)
}
\arguments{
  \item{x}{ object from class freqCalc }
  \item{method}{ approx (default) or exact }
  \item{qual}{ final correction factor }
  \item{survey}{ TRUE, if one have survey data and FALSE if one deal with the whole population. } 
}
\details{
Estimation of the risk for each observation. After the risk is computed one can use e.g. the function
localSuppr() for the protection of values of high risk. 
Further details can be found at the link given below.

S4 class sdcMicro objects are only supported by function \emph{measure_risk} that also estimates the 
individual risk with the same method.
}
\value{
  \item{rk }{ base individual risk }
  \item{method }{method}
  \item{qual }{final correction factor}
  \item{fk }{frequency count}
  \item{knames }{colnames of the key variables}  
}
\references{ 
    Franconi, L. and Polettini, S. (2004) \emph{Individual risk estimation in mu-Argus: a review}. Privacy in Statistical Databases, Lecture Notes in Computer Science, 262--272. Springer

    Machanavajjhala, A. and Kifer, D. and Gehrke, J. and Venkitasubramaniam, M. (2007) \emph{l-Diversity: Privacy Beyond k-Anonymity}.
 ACM Trans. Knowl. Discov. Data, 1(1)
 
 additionally, have a look at the vignettes of sdcMicro for further reading.
}
\author{ Matthias Templ. Bug in method \dQuote{exact} fixed since version 2.6.5. by Youri Baeyens.}
\note{ The base individual risk method was developed by Benedetti, Capobianchi and Franconi 
}
\seealso{  \code{\link{measure_risk}}, \code{\link{freqCalc}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
f
f$fk
f$Fk
## individual risk calculation:
indivf <- indivRisk(f)  
indivf$rk    
}
\keyword{ manip }
