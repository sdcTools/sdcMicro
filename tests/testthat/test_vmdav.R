# V-MDAV: microaggregation with variable group size (issue #311)
# Solanas, A. and Martinez-Balleste, A. (2006), COMPSTAT 2006.

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
# Example 1 of the paper: 13 records, 2 attributes
toy <- data.frame(
  x = c(2.4, 1.68, 3.18, 5.32, 18.68, 20.14, 19.85, 13.67, 17.11, 16.07, 21.28, 22, 23),
  y = c(3, 4.9, 5.54, 3.6, 11.49, 9.56, 12.33, 18.9, 21, 19.23, 18.9, 21, 18.5))
clustered <- function(seed = 1) {
  set.seed(seed)
  sizes <- rep(c(3, 4, 5), 20)
  cen <- cbind(runif(length(sizes), 0, 100), runif(length(sizes), 0, 100))
  as.data.frame(do.call(rbind, lapply(seq_along(sizes), function(i) {
    cbind(a = rnorm(sizes[i], cen[i, 1], 0.3), b = rnorm(sizes[i], cen[i, 2], 0.3))
  })))
}

test_that("vmdav finds the natural groups of the example in the paper", {
  # on the raw data, as in the paper
  g <- sdcMicro:::vmdav_partition(as.matrix(toy), k = 3, gamma = 1.1)
  truth <- c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
  expect_identical(match(g, unique(g)), match(truth, unique(truth)))
  # and through microaggregation(), which standardises the variables
  m <- microaggregation(toy, method = "vmdav", aggr = 3, gamma = 1.1)
  expect_identical(m$method, "vmdav")
  gv <- groups_of(m$mx)
  expect_identical(sort(as.integer(table(gv))), c(3L, 3L, 3L, 4L))
  gm <- groups_of(microaggregation(toy, method = "mdav", aggr = 3)$mx)
  expect_lt(sse(scale(toy), gv), sse(scale(toy), gm))
})

test_that("every group has at least k records and every record is in a group", {
  set.seed(42)
  for (n in c(3, 5, 6, 7, 20, 101)) {
    for (k in c(2, 3, 5)) {
      if (n < k) next
      x <- matrix(rnorm(n * 3), ncol = 3)
      for (gamma in c(0, 0.2, 1.1, 5)) {
        g <- sdcMicro:::vmdav_partition(x, k = k, gamma = gamma)
        info <- paste("n", n, "k", k, "gamma", gamma)
        expect_length(g, n)
        expect_false(anyNA(g), info = info)
        tab <- as.integer(table(g))
        expect_true(all(tab >= k), info = info)
        # groups are built with k to 2k - 1 records; fewer than k left-over
        # records are added to their closest groups at the end
        expect_true(all(tab <= 3 * k - 2), info = info)
        expect_true(sum(pmax(tab - (2 * k - 1), 0)) <= k - 1, info = info)
      }
    }
  }
})

test_that("gamma = 0 gives groups of fixed size", {
  set.seed(3)
  x <- matrix(rnorm(90 * 2), ncol = 2)
  expect_identical(as.integer(table(sdcMicro:::vmdav_partition(x, k = 3, gamma = 0))), rep(3L, 30))
  x <- matrix(rnorm(92 * 2), ncol = 2)
  tab <- as.integer(table(sdcMicro:::vmdav_partition(x, k = 3, gamma = 0)))
  expect_identical(sum(tab), 92L)
  expect_identical(length(tab), 30L)
})

test_that("vmdav adapts to clustered data", {
  x <- clustered()
  mv <- microaggregation(x, method = "vmdav", aggr = 3, gamma = 1.1)
  mm <- microaggregation(x, method = "mdav", aggr = 3)
  gv <- groups_of(mv$mx)
  expect_true(all(table(gv) >= 3))
  expect_true(length(unique(as.integer(table(gv)))) > 1)
  expect_lt(sse(scale(x), gv), 0.5 * sse(scale(x), groups_of(mm$mx)))
  # group means: totals are preserved, the input is returned unchanged
  expect_equal(colMeans(mv$mx), colMeans(x))
  expect_identical(mv$x, x)
  expect_identical(dim(mv$mx), dim(x))
  # deterministic
  expect_identical(mv$mx, microaggregation(x, method = "vmdav", aggr = 3, gamma = 1.1)$mx)
  # invariant to the scale of the variables
  x2 <- x
  x2$b <- x2$b * 1000
  expect_identical(groups_of(microaggregation(x2, method = "vmdav", aggr = 3, gamma = 1.1)$mx), gv)
})

test_that("vmdav works for a single variable, within strata and with missing values", {
  x <- clustered(2)
  m1 <- microaggregation(x, variables = "a", method = "vmdav", aggr = 3, gamma = 1.1)
  expect_identical(m1$method, "vmdav")
  expect_true(all(table(m1$mx$a) >= 3))

  x$s <- rep(c("u", "v"), length.out = nrow(x))
  ms <- microaggregation(x, variables = c("a", "b"), strata_variables = "s", method = "vmdav",
    aggr = 3, gamma = 1.1)
  expect_identical(colnames(ms$mx), c("a", "b"))
  for (s in c("u", "v")) {
    i <- x$s == s
    gs <- groups_of(ms$mx[i, ])
    expect_true(all(table(gs) >= 3))
    # no group across strata
    expect_equal(colMeans(ms$mx[i, ]), colMeans(x[i, c("a", "b")]))
  }

  xn <- clustered(3)
  xn$a[c(2, 50)] <- NA
  xn$b[7] <- NA
  mn <- microaggregation(xn, method = "vmdav", aggr = 3)
  expect_identical(unname(is.na(mn$mx)), unname(is.na(xn)))
  expect_false(anyNA(mn$mx[stats::complete.cases(xn), ]))
})

test_that("vmdav works for sdcMicroObj objects", {
  data(Tarragona)
  nv <- c("CURRENT.ASSETS", "SALES", "NET.PROFIT")
  sdc <- createSdcObj(Tarragona, numVars = nv)
  sdc <- microaggregation(sdc, method = "vmdav", aggr = 3, gamma = 1.1)
  mx <- get.sdcMicroObj(sdc, "manipNumVars")
  expect_true(all(table(groups_of(mx)) >= 3))
  expect_equal(colMeans(mx), colMeans(Tarragona[, nv]))
  # other methods are not affected by the new argument
  set.seed(1)
  x <- data.frame(a = rnorm(60), b = rnorm(60))
  expect_identical(microaggregation(x, method = "mdav", aggr = 3, gamma = 5)$mx,
    microaggregation(x, method = "mdav", aggr = 3)$mx)
})

test_that("vmdav checks its arguments", {
  x <- clustered()
  expect_error(microaggregation(x, method = "vmdav", gamma = -1), "gamma")
  expect_error(microaggregation(x, method = "vmdav", gamma = c(1, 2)), "gamma")
  expect_error(microaggregation(x, method = "vmdav", gamma = NA), "gamma")
  expect_error(microaggregation(x[1:2, ], method = "vmdav", aggr = 3), "at least")
  x$f <- "a"
  expect_error(microaggregation(x, variables = c("a", "f"), method = "vmdav"), "numeric")
})

test_that("vmdav reproduces the information loss reported in the paper", {
  # Table 1 of Solanas and Martinez-Balleste (2006): 100 * SSE / SST on the
  # standardised data, for k = 3, 4, 5, 10
  iloss <- function(x, k, gamma) {
    x <- scale(as.matrix(x))
    g <- sdcMicro:::vmdav_partition(x, k = k, gamma = gamma)
    100 * sse(x, g) / sum(x^2)
  }
  ks <- c(3, 4, 5, 10)
  # "Census": with gamma = 0 V-MDAV is the fixed-size method (column MDAV)
  data(CASCrefmicrodata)
  expect_equal(vapply(ks, function(k) iloss(CASCrefmicrodata, k, 0), numeric(1)),
    c(5.66, 7.51, 9.01, 14.07), tolerance = 0.006, scale = 1)
  # "Tarragona": V-MDAV with gamma = 0.2, the value used for scattered data
  data(Tarragona)
  expect_equal(vapply(ks, function(k) iloss(Tarragona, k, 0.2), numeric(1)),
    c(16.96, 19.70, 22.88, 33.26), tolerance = 0.015, scale = 1)
})
