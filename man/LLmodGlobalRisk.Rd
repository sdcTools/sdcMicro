\name{LLmodGlobalRisk}
\docType{methods}
\alias{LLmodGlobalRisk-methods}
\alias{LLmodGlobalRisk,ANY-method}
\alias{LLmodGlobalRisk,data.frame-method}
\alias{LLmodGlobalRisk,matrix-method}
\alias{LLmodGlobalRisk,sdcMicroObj-method}
\alias{LLmodGlobalRisk}
\title{
Global risk using log-linear models.
}
\description{
The sample frequencies are assumed to be independent and following a 
Poisson distribution. The parameters of the corresponding parameters are
estimated by a log-linear model including the main effects and possible interactions.
}
\usage{
LLmodGlobalRisk(obj, method = "IPF", inclProb = NULL, form = NULL, modOutput = FALSE)
}
\arguments{
  \item{obj}{
An object of class sdcMicroObj or a numeric matrix or data frame containing the categorical key variables.
}
  \item{method}{
At this time, only iterative proportional fitting  (\dQuote{IPF}) can be used.
}
  \item{inclProb}{
Inclusion probabilites (experimental)
}
\item{form}{
A formula specifying the model.
}
  \item{modOutput}{
If TRUE, additional output is given.
}
}
\details{

This measure aims to (1) calculate the number of sample uniques that are population uniques with a probabilistic Poisson model and (2) to estimate the expected number of correct matches for sample uniques.

ad 1) this risk measure is defined over all sample uniques (SU) as 
\deqn{
\tau_1 = \sum\limits_{SU} P(F_k=1 | f_k=1) \quad ,
}
i.e. the expected number of sample uniques that are population uniques.

ad 2) this risk measure is defined over all sample uniques (SU) as
\deqn{
\tau_2 = \sum\limits_{SU} P(F_k=1 | f_k=1) \quad , CORRECT!
}

Since population frequencies \eqn{F_k} are unknown, they has to be estimated.

The iterative proportional fitting method is used to fit the parameters of 
the Poisson distributed frequency counts related to the model specified to fit the frequency counts. 
The obtained parameters are used to estimate a global risk, defined in Skinner and Holmes (1998). 

}
\value{
Two global risk measures or the modified risk in the \dQuote{sdcMicroObj} object.
}
\references{
Skinner, C.J. and Holmes, D.J. (1998) \emph{Estimating the re-identification risk per record in microdata}.
Journal of Official Statistics, 14:361-372, 1998.

Rinott, Y. and Shlomo, N. (1998). \emph{A Generalized Negative Binomial Smoothing Model for Sample Disclosure
  Risk Estimation}. Privacy in Statistical Databases. Lecture Notes in Computer Science.
	Springer-Verlag, 82--93.
}
\author{
Matthias Templ
}
\seealso{
\code{\link{loglm}}, \code{\link{measure_risk}}
}
\examples{
data(testdata2)
x <- testdata2[,c("sex","water","roof")]
res <- LLmodGlobalRisk(x, form=~sex+water+roof, 
           inclProb=1/testdata2[,"sampling_weight"])
res$gr1; res$gr2

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- LLmodGlobalRisk(sdc,form=~sex+water+roof)
}
\keyword{ manip }
