#' Comparison plots
#'
#' Plots for the comparison of the original data and perturbed data.
#'
#' Univariate and multivariate comparison plots are implemented to detect
#' differences between the perturbed and the original data, but also to compare
#' perturbed data which are produced by different methods.
#'
#' @md
#' @param x an output object of [microaggregation()]
#' @param p necessary parameter for the box cox transformation (`lambda`)
#' @param which.plot which plot should be created?
#' - `1`: density traces
#' - `2`: parallel boxplots
#' - `3`: differences in totals
#' @author Matthias Templ
#' @seealso [microaggregation()]
#' @references Templ, M. and Meindl, B., *Software Development for SDC in
#' R*, Lecture Notes in Computer Science, Privacy in Statistical Databases,
#' vol. 4302, pp. 347-359, 2006.
#' @keywords aplot
#' @return returns `NULL`; the selected plot is displayed
#' @export
#' @examples
#' data(free1)
#' df <- as.data.frame(free1)[, 31:34]
#' m1 <- microaggregation(df, method = "onedims", aggr = 3)
#' plotMicro(m1, p = 1, which.plot = 1)
#' plotMicro(m1, p = 1, which.plot = 2)
#' plotMicro(m1, p = 1, which.plot = 3)
plotMicro <- function(x, p, which.plot = 1:3) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  bct <- function(y, p) {
    gm <- exp(colMeans(log(y)))
    if (p == 0)
      return(log(y) * gm)
    (y^p - 1)/(p * gm^(p - 1))
  }

  x1 <- bct(x$x, p)
  x2 <- bct(x$mx, p)
  if (length(which.plot) > 1)
    par(ask = TRUE)
  if (1 %in% which.plot) {
    par(mfrow = c(2, 2), mar = c(3, 2, 2, 1))
    n <- dim(x1)[2]
    if (n == 1) {
      r1 <- 1
      r2 <- 2
    }
    if (n == 2) {
      r1 <- 1
      r2 <- 2
    }
    if (n > 2 && n < 5) {
      r1 <- 2
      r2 <- 2
    }
    if (n >= 5 && n < 7) {
      r1 <- 2
      r2 <- 3
    }
    if (n >= 7 && n < 10) {
      r1 <- 3
      r2 <- 3
    }
    if (n >= 10 && n < 13) {
      r1 <- 3
      r2 <- 4
    }
    if (n >= 13 && n < 17) {
      r1 <- 4
      r2 <- 4
    }
    if (n >= 17) {
      r1 <- 4
      r2 <- n/4
    }
    par(mfrow = c(r1, r2))
    for (i in 1:dim(x1)[2]) {
      plot(stats::density(x1[, i]), main = paste(colnames(x1)[i], "- density traces"))
      lines(stats::density(x2[, i]), col = "red")
      legend("topright", legend = c("original data", "microaggr. data"), lty = c(1, 1),
        lwd = c(1, 1), col = c("black", "red"))
    }
  }
  if (2 %in% which.plot) {
    par(mfrow = c(2, 2), mar = c(3, 2, 2, 1))
    x1d <- data.frame(x1)
    x2d <- data.frame(x2)
    b <- boxplot(x1d, boxwex = 0.2, las = 3, col = "yellow", ylab = paste("boxcox-transformated data (lambda = ",
      p, ")", sep = ""), main = "Boxplots")
    boxplot(x2d, add = TRUE, at = 1:dim(x$x)[2] + 0.3, boxwex = 0.2, xaxt = "n", yaxt = "n",
      col = "orange")
    legend("topright", legend = c("original data", "microaggr. data"), pch = c(15, 15),
      col = c("yellow", "orange"))
    if (dim(x$mx)[1] > dim(x$mx)[2]) {
      pc1 <- stats::princomp(scale(x$x))
      xm <- x$mx
      colnames(xm) <- colnames(x$x)
      pc2 <- stats::princomp(scale(xm))
      stats::biplot(pc1)
      mtext("Original data", 3)
      stats::biplot(pc2)
      mtext("Microaggregated data", 3)
    }
  }
  if (3 %in% which.plot) {
    groups <- colnames(x$x)
    x1 <- as.data.frame(x$x)
    x2 <- as.data.frame(x$mx)
    colnames(x2) <- colnames(x1)
    totx <- apply(x1, 2, sum)
    totxm <- apply(x2, 2, sum)
    Difference <- ((totxm - totx)/totx) * 100
    wx <- which(Difference < 0)
    wxm <- which(Difference >= 0)
    d1 <- rep(0, dim(x$x)[2])
    d1[wx] <- Difference[wx]
    d2 <- rep(0, dim(x$x)[2])
    d2[wxm] <- Difference[wxm]
    par(mar = c(0.5, 5, 0.5, 1), mfrow = c(1, 1))
    plot.new()
    plot.window(xlim = c(-10, 10), ylim = c(-1.5, length(groups)))
    ticks <- seq(-10, 10, 2)
    y <- 1:length(groups)
    h <- 0.3
    lines(rep(0, 2), c(-1.5, 5.5), col = "grey")
    segments(-10, y, 10, y, lty = "dotted")
    rect(-d1, y - h, 0, y + h, col = "dark grey")
    rect(0, y - h, d2, y + h, col = "light grey")
    mtext(groups, at = y, adj = 1, side = 2, las = 2)
    par(cex.axis = 0.5, mex = 0.5)
    axis(1, at = ticks, labels = abs(ticks), pos = 0)
    tw <- 1.5 * strwidth("Minus")
    rect(-tw, -1 - h, 0, -1 + h, col = "dark grey")
    rect(0, -1 - h, tw, -1 + h, col = "light grey")
    text(0, -1, "Minus", pos = 2)
    text(0, -1, "Plus", pos = 4)
    text(0, 17, "Difference of Totals before and after Microaggregation in percent")
    box("inner", col = "grey")
  }
}
