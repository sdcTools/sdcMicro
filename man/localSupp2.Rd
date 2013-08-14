\name{localSupp2}
\alias{localSupp2}
\title{ Local Suppression 2 }
\description{
An Algorithm to perfom local suppression to achieve k-anonymity.
}
\usage{
localSupp2(x, keyVars, w, importance=rep(1, length(keyVars)),
  method="minimizeSupp", k=1)
}
\arguments{
  \item{x}{ data frame or matrix }
  \item{keyVars}{ column index of key variables }
  \item{w}{ column index of sampling weights }
  \item{importance}{ weights for each key variable }
  \item{method}{ \dQuote{minimizeSupp} (default), further methods will be included in future versions of the package }
  \item{k}{ parameter for k-anonymity. }
}
\details{
With the help of this algorithm you can achieve k-anonymity in an optimized way. The procedure set missings only to those 
key variables for which the importance is greater than 0. Key variables with higher importance will be prefered to be the variable which 
will used for suppression of specific values, i.e. the vector of importance assign to each key variables a weight which is considered by
the algorithm.

To guarantee k-anonymity the wrapper of function localSupp2 should be applied (localSupp2Wrapper())

However, if the importance of some key variables are equal to zero, 
the alorithm may not find a k-anonymity solution (because there isn't any solution
reachable at all, for example). The easiest way to overcome this
situation is to re-run the alorithm and allow for NA's in some more key variables, 
i.e. re-run the algorithm with importance greater than 
0 for all entries of importance. 
This will result in k-anonymizated results and leads to only few suppressions 
in the key variables where the importance of the variables are considered. 

Method fastSupp avoids some calculation steps but this method is only significant 
faster if there is a large data sets with few key variables.
However, fastSupp leads to an oversuppression (sligthly).  
}
\value{
  Object from class localSupp2.
  \item{xAnon}{resulting data with suppressions}
  \item{supps }{number of suppressions in the key variables }
  \item{totalSupps}{total number of suppressions.}
    \item{anonymity }{ TRUE, if k-anonymity is achieved }
    \item{keyVars }{index of the key variables.}
    \item{importance}{weight vector for key variables}
    \item{k }{k for k-anoymity}    
}
\references{ 
Templ, M.  
               \emph{Statistical Disclosure Control for Microdata Using the R-Package sdcMicro}, 
               Transactions on Data Privacy, 
               vol. 1, number 2, pp. 67-85, 2008. 
  \url{http://www.tdp.cat/issues/abs.a004a08.php}
}
\author{ Matthias Templ, Bernhard Meindl }
\note{ fix me: Implementation in C and interface to R.
}
\seealso{  \code{\link{freqCalc}}, \code{\link{localSuppression}} }
\examples{
print("this function is deprecated, please use localSuppression() instead")
}
\keyword{ manip }
