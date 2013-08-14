\name{suda2}
\docType{methods}
\alias{suda2-methods}
\alias{suda2,data.frame-method}
\alias{suda2,matrix-method}
\alias{suda2,sdcMicroObj-method}
\alias{suda2}
\title{
Suda2: Detecting Special Uniques
}
\description{
SUDA risk measure for data from (stratified) simple random sampling.
}
\usage{
suda2(obj,...)#,variables=NULL,missing=-999,DisFraction=0.01)
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "data.frame")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "matrix")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "sdcMicroObj")}}{
%%  ~~describe this method here~~
}
}}
\arguments{
  \item{obj}{
object of class \dQuote{data.frame} or object of class \dQuote{sdcMicroObj}
}
\item{...}{see arguments below}
  \item{variables}{
Categorical (key) variables. Either the column names or and index of the variables to be used for
risk measurement.
}
  \item{missing}{
Missing value coding in the given data set.
}
  \item{DisFraction}{
It is the sampling fraction for the simple random sampling, and the common 
sampling fraction for stratified sampling. By default, it's set to 0.01.
}
}
\details{
Suda 2 is a recursive algorithm for finding
Minimal Sample Uniques. The algorithm generates all possible
variable subsets of defined categorical key variables 
and scans them for unique patterns in the subsets of variables. The lower the
amount of variables needed to receive uniqueness, the higher the risk of the
corresponding observation.
}
\value{
A modified \dQuote{sdcMicroObj} object or the following list
\item{ContributionPercent}{The contribution of each key variable to the SUDA score, calculated for each row.}
\item{score }{The suda score.}
\item{disscore }{The dis suda score}
}
\references{
C. J. Skinner; M. J. Elliot (20xx)
A Measure of Disclosure Risk for Microdata. \emph{Journal of the Royal 
Statistical Society: Series B (Statistical Methodology)}, Vol. 64 (4), pp 855--867.
 
M. J. Elliot, A. Manning, K. Mayes, J. Gurd and M. Bane (20xx)
SUDA: A Program for Detecting Special Uniques, 
Using DIS to Modify the Classification of Special Uniques

Anna M. Manning, David J. Haglin, John A. Keane (2008)
A recursive search algorithm for statistical disclosure assessment.
\emph{Data Min Knowl Disc}  16:165 -- 196 
}
\author{
Alexander Kowarik based on the C++  code from the 
Organisation For Economic Co-Operation And Development.  

For the C++ code: 
This work is being supported by the International Household
Survey Network and funded by a DGF Grant provided by
the World Bank to the PARIS21 Secretariat at the Organisation for Economic
Co-operation and Development (OECD). This work builds on previous work which
is elsewhere acknowledged.
}
\examples{
data(testdata2)
data_suda2 <- suda2(testdata2,variables=c("urbrur","roof","walls","water","sex"))
data_suda2
summary(data_suda2)

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- suda2(sdc)
}
\keyword{ manip }
