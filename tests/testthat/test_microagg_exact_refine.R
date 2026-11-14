# Optimal univariate microaggregation (method "onedimsopt") and the refinement
# step (argument "refine") of microaggregation().
# Hansen, S.L. and Mukherjee, S. (2003), IEEE TKDE 15(4), 1043-1044.

# group labels from a microaggregated data set: records of a group share all values
groups_of <- function(mx) {
  key <- do.call(paste, c(as.data.frame(mx), sep = "\r"))
  match(key, unique(key))
}
sse <- function(x, g) {
  x <- as.matrix(x)
  sum(vapply(split(seq_len(nrow(x)), g), function(i) {
    sum(sweep(x[i, , drop = FALSE], 2, colMeans(x[i, , drop = FALSE]))^2)
  }, numeric(1)))
}
std <- function(x) {
  x <- as.matrix(x)
  sweep(sweep(x, 2, colMeans(x)), 2, apply(x, 2, stats::sd), "/")
}
# smallest SSE over ALL partitions of x into groups of at least k values
# (not only those that are contiguous in the sorted values)
brute_sse <- function(x, k) {
  n <- length(x)
  best <- Inf
  rec <- function(i, g, ng) {
    if (i > n) {
      if (all(tabulate(g, ng) >= k)) {
        best <<- min(best, sse(matrix(x), g))
      }
      return(invisible())
    }
    for (h in seq_len(ng + 1L)) {
      g[i] <- h
      rec(i + 1L, g, max(ng, h))
    }
  }
  rec(1L, integer(n), 0L)
  best
}
clustered <- function(seed = 1) {
  set.seed(seed)
  sizes <- rep(c(3, 4, 5), 20)
  cen <- cbind(runif(length(sizes), 0, 100), runif(length(sizes), 0, 100))
  as.data.frame(do.call(rbind, lapply(seq_along(sizes), function(i) {
    cbind(a = rnorm(sizes[i], cen[i, 1], 0.3), b = rnorm(sizes[i], cen[i, 2], 0.3))
  })))
}

## ---- optimal univariate microaggregation ------------------------------------

test_that("onedimsopt_partition finds the optimum over all partitions", {
  for (seed in 1:6) {
    set.seed(seed)
    for (k in 2:3) {
      n <- if (k == 2) 8 else 9
      x <- if (seed %% 2 == 0) round(rexp(n), 1) else rnorm(n)
      g <- sdcMicro:::onedimsopt_partition(x, k = k)
      expect_true(all(table(g) >= k))
      expect_equal(sse(matrix(x), g), brute_sse(x, k), tolerance = 1e-10)
    }
  }
})

test_that("onedimsopt groups are contiguous, of size k to 2k - 1, and beat fixed-size groups", {
  set.seed(2)
  x <- c(rnorm(40), rnorm(31, 10), rexp(52))
  for (k in c(2, 3, 5, 10)) {
    g <- sdcMicro:::onedimsopt_partition(x, k = k)
    tab <- table(g)
    expect_true(all(tab >= k & tab <= 2 * k - 1))
    # contiguous in the sorted values: the group label never changes back
    gs <- g[order(x)]
    expect_identical(length(rle(gs)$lengths), length(tab))
    fixed <- pmin(ceiling(rank(x, ties.method = "first") / k), floor(length(x) / k))
    expect_lt(sse(matrix(x), g), sse(matrix(x), fixed))
  }
  # no restriction that hurts: k = 1 keeps every value
  expect_equal(sse(matrix(x), sdcMicro:::onedimsopt_partition(x, k = 1)), 0)
  # constant values and heavy ties
  expect_true(all(table(sdcMicro:::onedimsopt_partition(rep(5, 10), k = 3)) >= 3))
  xt <- rep(c(1, 2, 7), c(4, 5, 3))
  expect_equal(sse(matrix(xt), sdcMicro:::onedimsopt_partition(xt, k = 3)), 0)
})

test_that("onedimsopt stays exact for large values with a small spread", {
  set.seed(5)
  x <- 1e9 + rnorm(9)
  g <- sdcMicro:::onedimsopt_partition(x, k = 3)
  expect_equal(sse(matrix(x - 1e9), g), brute_sse(x - 1e9, 3), tolerance = 1e-8)
})

test_that("microaggregation(method = 'onedimsopt') treats each variable on its own", {
  set.seed(3)
  x <- data.frame(a = rexp(101), b = rnorm(101), c = runif(101))
  m <- microaggregation(x, method = "onedimsopt", aggr = 3)
  expect_s3_class(m, "micro")
  expect_identical(m$method, "onedimsopt")
  expect_identical(dim(m$mx), dim(x))
  for (v in names(x)) {
    g <- sdcMicro:::onedimsopt_partition(x[[v]], k = 3)
    expect_equal(m$mx[[v]], stats::ave(x[[v]], g))
    expect_true(all(table(m$mx[[v]]) >= 3))
    expect_equal(mean(m$mx[[v]]), mean(x[[v]]))
  }
  # never worse than individual ranking with groups of fixed size
  mo <- microaggregation(x, method = "onedims", aggr = 3)
  expect_lt(sum((x - m$mx)^2), sum((x - mo$mx)^2))
  # a single variable is not handed to mafast()
  m1 <- microaggregation(x, variables = "a", method = "onedimsopt", aggr = 3)
  expect_identical(m1$method, "onedimsopt")
  expect_equal(m1$mx$a, m$mx$a)
})

test_that("onedimsopt works within strata, with missing values and for sdcMicroObj", {
  set.seed(4)
  x <- data.frame(a = rexp(60), b = rnorm(60), s = rep(c("u", "v", "w"), c(10, 20, 30)))
  ms <- microaggregation(x, variables = c("a", "b"), strata_variables = "s",
    method = "onedimsopt", aggr = 3)
  for (lev in unique(x$s)) {
    i <- x$s == lev
    ref <- microaggregation(x[i, c("a", "b")], method = "onedimsopt", aggr = 3)$mx
    expect_equal(ms$mx$a[i], ref$a)
    expect_equal(ms$mx$b[i], ref$b)
  }
  xn <- x[, c("a", "b")]
  xn$a[c(2, 17)] <- NA
  mn <- microaggregation(xn, method = "onedimsopt", aggr = 3)
  expect_identical(is.na(mn$mx$a), is.na(xn$a))
  expect_equal(mn$mx$a[!is.na(xn$a)],
    stats::ave(xn$a[!is.na(xn$a)], sdcMicro:::onedimsopt_partition(xn$a[!is.na(xn$a)], 3)))
  expect_equal(mn$mx$b, microaggregation(x[, c("a", "b")], method = "onedimsopt", aggr = 3)$mx$b)

  data(testdata2)
  sdc <- createSdcObj(testdata2, keyVars = c("urbrur", "roof", "walls"),
    numVars = c("expend", "income", "savings"), w = "sampling_weight")
  sdc <- microaggregation(sdc, method = "onedimsopt", aggr = 3)
  mm <- get.sdcMicroObj(sdc, "manipNumVars")
  ref <- microaggregation(testdata2[, c("expend", "income", "savings")],
    method = "onedimsopt", aggr = 3)$mx
  expect_equal(mm$expend, ref$expend)
  expect_equal(mm$savings, ref$savings)
})

test_that("onedimsopt checks its arguments", {
  x <- data.frame(a = c(1, 2, 3, 4, 5, 6), b = 6:1, f = letters[1:6])
  expect_error(microaggregation(x[1:2, c("a", "b")], method = "onedimsopt", aggr = 3), "at least")
  expect_error(microaggregation(x, variables = c("a", "f"), method = "onedimsopt"), "numeric")
  expect_error(microaggregation(x[, c("a", "b")], method = "onedimsopt", aggr = 0), "aggr")
})

## ---- refinement step ----------------------------------------------------------

test_that("microagg_refine never increases the SSE and keeps groups of at least k", {
  x <- std(clustered(3))
  for (k in c(2, 3, 4)) {
    g0 <- rep(seq_len(floor(nrow(x) / k)), each = k, length.out = nrow(x))
    g0[seq_len(nrow(x)) > floor(nrow(x) / k) * k] <- floor(nrow(x) / k)
    g1 <- sdcMicro:::microagg_refine(x, g0, k = k)
    expect_true(all(table(g1) >= k))
    expect_lt(sse(x, g1), sse(x, g0))
  }
})

test_that("microagg_refine stops at a partition that no move and no swap improves", {
  set.seed(6)
  x <- std(matrix(rnorm(60), ncol = 2))
  k <- 3
  g <- sdcMicro:::microagg_refine(x, rep(1:10, each = 3), k = k)
  s0 <- sse(x, g)
  size <- table(g)
  best <- 0
  for (i in seq_len(nrow(x))) {
    for (h in setdiff(unique(g), g[i])) {
      if (size[[as.character(g[i])]] > k && size[[as.character(h)]] < 2 * k - 1) {
        g2 <- g
        g2[i] <- h
        best <- min(best, sse(x, g2) - s0)
      }
    }
    for (j in which(g != g[i])) {
      g2 <- g
      g2[c(i, j)] <- g[c(j, i)]
      best <- min(best, sse(x, g2) - s0)
    }
  }
  expect_gt(best, -1e-8)
})

test_that("refine = TRUE lowers the information loss of the multivariate methods", {
  x <- clustered(2)
  xs <- std(x)
  for (method in c("mdav", "vmdav", "pca", "simple", "rmd")) {
    m0 <- microaggregation(x, method = method, aggr = 3)
    m1 <- microaggregation(x, method = method, aggr = 3, refine = TRUE)
    expect_identical(m1$method, method)
    g0 <- groups_of(m0$mx)
    g1 <- groups_of(m1$mx)
    expect_true(all(table(g1) >= 3), info = method)
    expect_lte(sse(xs, g1), sse(xs, g0) + 1e-8)
    # the refined groups are replaced by their means
    expect_equal(m1$mx$a, stats::ave(x$a, g1), info = method)
    expect_equal(m1$mx$b, stats::ave(x$b, g1), info = method)
  }
  # 'simple' groups the records as they come: a lot to gain
  expect_lt(sse(xs, groups_of(microaggregation(x, method = "simple", aggr = 3, refine = TRUE)$mx)),
    0.5 * sse(xs, groups_of(microaggregation(x, method = "simple", aggr = 3)$mx)))
})

test_that("the default is unchanged and refine is reproducible", {
  data(Tarragona)
  x <- as.data.frame(Tarragona)[1:200, 1:5]
  expect_identical(microaggregation(x, method = "mdav", aggr = 3)$mx,
    microaggregation(x, method = "mdav", aggr = 3, refine = FALSE)$mx)
  m1 <- microaggregation(x, method = "mdav", aggr = 3, refine = TRUE)
  expect_identical(m1$mx, microaggregation(x, method = "mdav", aggr = 3, refine = TRUE)$mx)
  expect_lt(sse(std(x), groups_of(m1$mx)),
    sse(std(x), groups_of(microaggregation(x, method = "mdav", aggr = 3)$mx)))
})

test_that("refine works within strata and for sdcMicroObj", {
  set.seed(7)
  x <- data.frame(a = rexp(90), b = rnorm(90), s = rep(c("u", "v"), c(30, 60)))
  ms <- microaggregation(x, variables = c("a", "b"), strata_variables = "s",
    method = "mdav", aggr = 3, refine = TRUE)
  for (lev in unique(x$s)) {
    i <- x$s == lev
    ref <- microaggregation(x[i, c("a", "b")], method = "mdav", aggr = 3, refine = TRUE)$mx
    expect_equal(ms$mx$a[i], ref$a)
    expect_true(all(table(groups_of(ms$mx[i, ])) >= 3))
  }
  data(testdata2)
  sdc <- createSdcObj(testdata2, keyVars = c("urbrur", "roof", "walls"),
    numVars = c("expend", "income", "savings"), w = "sampling_weight")
  s1 <- microaggregation(sdc, method = "mdav", aggr = 3, refine = TRUE)
  ref <- microaggregation(testdata2[, c("expend", "income", "savings")],
    method = "mdav", aggr = 3, refine = TRUE)$mx
  expect_equal(get.sdcMicroObj(s1, "manipNumVars")$income, ref$income)
})

test_that("refine checks its arguments", {
  set.seed(8)
  x <- data.frame(a = rexp(30), b = rnorm(30))
  expect_error(microaggregation(x, method = "mdav", refine = NA), "refine")
  expect_error(microaggregation(x, method = "mdav", refine = "yes"), "refine")
  expect_error(microaggregation(x, method = "onedims", refine = TRUE), "onedims")
  expect_error(microaggregation(x, method = "onedimsopt", refine = TRUE), "onedimsopt")
  expect_error(microaggregation(x, variables = "a", method = "mdav", refine = TRUE), "onedimsopt")
  xn <- x
  xn$a[3] <- NA
  expect_error(microaggregation(xn, method = "vmdav", refine = TRUE), "missing")
})
