`swappNum` <-
function(x, w=1:(dim(x)[2]), p){
.Deprecated("rankSwap")
  xx <- x[,w]
  y=x
  if( class(xx) == "matrix" || class(xx) == "data.frame" ){
    N <- dim(xx)[1] } else { N <- length(xx) }
  ranges <- round(p*N/100)

  swapping <- function(xx){
    ### swapp one variable:
    r <- rank(xx)
    s <- sort(xx, index.return=TRUE)
#    change <- function(s){
      index <- 1:length(s$x)
      z <- s$x
      ss <- s$x
      for( i in 1:N ){
        actual.index <- i
        ### Bereich des swappens:
        mi <- min( c(actual.index, actual.index - ranges) )
        if( mi < 1 ) mi <- 1
        ma <- min( c(actual.index + ranges, N) )
        ### Austausch von z[i] mit anderem z[sNew]
        if( length(mi:ma) == 1 ) { sNew <- i
        } else{ sNew <- sample(c(mi:ma), 1)}
        z[i] <- ss[sNew]
        z[sNew] <- ss[i]
        ss <- z
      }
      z
#    }
    z[r]  ## rueckordnen
  }
  # Univariat
  if(length(w)==1)
	  x[,w] <- swapping(xx)
  ### Multivariate:
  else
  	x[,w] <- apply(xx, 2, swapping)
  res <- list(x=y, xm=x, method="swappNum", p=p)
  class(res) <- "micro"
  res
}

