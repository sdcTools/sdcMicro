\name{microData}
\alias{microData}
\docType{data}
\title{ microData }
\description{
Small aritificial toy data set.
}
\usage{data(microData)}
\format{
  The format is:
 num [1:13, 1:5] 5 7 2 1 7 8 12 3 15 4 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:13] "10000" "11000" "12000" "12100" ...
  ..$ : chr [1:5] "one" "two" "three" "four" ...
}
\examples{
data(microData)
m1 <- microaggregation(microData, method="mdav")
summary(m1)
}
\keyword{datasets}
