\name{rankSwap}
\docType{methods}
\alias{rankSwap-methods}
\alias{rankSwap,data.frame-method}
\alias{rankSwap,matrix-method}
\alias{rankSwap,sdcMicroObj-method}
\alias{rankSwap}
\title{
Rank Swapping
}
\description{
Swapping values within a range so that, first, the correlation structure of original variables are
preserved, and second, the values in each record are disturbed.
To be used on numeric or ordinal variables where the rank can be determined and the correlation
coefficient makes sense.
}
\usage{
rankSwap(obj, variables=NULL,TopPercent=5,BottomPercent=5,
        K0=-1,R0=.95,P=0,missing=-999,seed=NULL)
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
object of class sdcMicroObj or matrix or data frame
}
  \item{variables}{
names or index of variables for that rank swapping is applied.
For an object of class 'sdcMicroObj' all numeric key variables are selected if variables=NULL.
}
  \item{TopPercent}{
Percentage of largest values that are grouped together before rank swapping is applied.
}
  \item{BottomPercent}{
Percentage of lowest values that are grouped together before rank swapping is applied.
}
  \item{K0}{
Subset-mean preservation factor. Preserves the means before and after rank swapping
within a range based on K0.
K0 is the subset-mean preservation factor such that
\eqn{| X_1 -X_2  | \leq \frac{2 K_0 X_1}{\sqrt(N_S)}}{abs(X_1-X_2<=2*K_0*X_1/sqrt (N_S)}, 
where \eqn{X_1}{X_1} and \eqn{X_2}{X_2} are the subset means of the field before and after swapping,
and \eqn{N_S}{N_S} is the sample size of the subset. 
}
  \item{R0}{
Multivariate preservation factor. Preserves the correlation between variables
within a certain range based on the given constant R0.
We can specify the preservation factor as \eqn{R_0 = \frac{R_1}{R_2}}{R_0 = R_1/R_2} where
\eqn{R_1}{R_1} is the correlation coefficient of the two fields after swapping, and \eqn{R_2}{R_2}
is the correlation coefficient of the two fields before swapping.
}
\item{P}{
Rank range as percentage of total sample size. We can specify the rank range itself directly,
noted as \eqn{P}{P}, which is the percentage of the records. So two records are eligible
for swapping if their ranks, \eqn{i}{i} and \eqn{j}{j} respectively, satisfy
\eqn{| i-j | \le \frac{P N}{100}}{abs(i-j)<P*N/100}, where \eqn{N}{N} is the total
sample size.
}
  \item{missing}{
missig value code.
}
  \item{seed}{
Seed.
}
}
\details{
Rank swapping sorts the values of one numeric variable by their
numerical values (ranking).  
The restricted range is determined by the rank of two swapped values, which
cannot differ, by definition, by more than \eqn{P}{P} percent of the total number of
observations.
R0 and K0 are only used if positive. Only one of the two are used (R0 is prefered if both are positive).


}
\value{
The rank-swapped data set or a modified \dQuote{sdcMicroObj} object.
}
\references{
Moore, Jr.R. (1996) 
    Controlled data-swapping techniques for masking public use microdata,
    U.S. Bureau of the Census
    \emph{Statistical Research Division Report Series},
    RR 96-04 .
}
\author{Alexander Kowarik for the interface.

For the underlying C++ code: This work is being supported by the International Household 
Survey Network (IHSN) and funded by a DGF Grant provided by the World Bank to the 
PARIS21 Secretariat at the Organisation for Economic Co-operation and Development (OECD). 
This work builds on previous work which is elsewhere acknowledged. 
}

\examples{
data(testdata2)
data_swap <- rankSwap(testdata2,variables=c("age","income","expend","savings"))

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- rankSwap(sdc)
}
