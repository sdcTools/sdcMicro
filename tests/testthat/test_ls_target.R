# TDD for pluggable protection targets. Everything a solver needs from a
# count-based criterion is a per-record threshold: record i must reach a
# released frequency of at least k_i. k-anonymity is the constant case;
# individual-risk thresholds are the interesting one, because with survey
# weights the required frequency differs per record. ls_target() turns a
# criterion into that vector, delegating every risk evaluation to
# indivRisk() rather than reimplementing the formulas.

test_that("ls_target reproduces k-anonymity as a constant vector", {
  x <- data.frame(a = c("1", "1", "2"), b = c("1", "2", "2"),
                  stringsAsFactors = FALSE)
  expect_equal(as.vector(ls_target(x, c("a", "b"), kanon = 3)), rep(3L, 3L))
  expect_equal(attr(ls_target(x, c("a", "b"), kanon = 3), "criterion"), "kanon")
})

test_that("ls_target maps a population risk threshold to 1/tau", {
  # Without weights sdcMicro's individual risk is 1/f, so r <= tau is
  # exactly f >= ceiling(1/tau), capped at the number of records.
  x <- data.frame(a = as.character(1:50), b = as.character(1:50),
                  stringsAsFactors = FALSE)
  expect_equal(unique(ls_target(x, c("a", "b"), risk = 0.5)), 2L)
  expect_equal(unique(ls_target(x, c("a", "b"), risk = 0.2)), 5L)
  expect_equal(unique(ls_target(x, c("a", "b"), risk = 0.03)), 34L)
  small <- x[1:10, ]
  expect_equal(unique(ls_target(small, c("a", "b"), risk = 0.03)), 10L)
})

test_that("ls_target agrees with indivRisk on weighted data", {
  skip_if_not_installed("sdcMicro")
  set.seed(4)
  n <- 60L
  x <- data.frame(a = sample(c("1", "2"), n, TRUE),
                  b = sample(c("1", "2", "3"), n, TRUE),
                  w = runif(n, 5, 50), stringsAsFactors = FALSE)
  tau <- 0.05
  kv <- ls_target(x, c("a", "b"), risk = tau, w = "w")
  # the returned threshold must be the smallest frequency at which sdcMicro's
  # own risk function drops to tau, given each record's original weighted count
  fc <- freqCalc(x, keyVars = c("a", "b"), w = 3)
  for (i in sample(seq_len(n), 8)) {
    f_try <- if (kv[i] > 1L) c(kv[i] - 1L, kv[i]) else kv[i]
    o <- list(freqCalc = x[seq_along(f_try), c("a", "b")],
              keyVars = c("a", "b"),
              fk = f_try, Fk = rep(fc$Fk[i], length(f_try)))
    class(o) <- "freqCalc"
    r <- indivRisk(o)$rk
    expect_true(r[length(r)] <= tau + 1e-12)     # the threshold suffices
    if (kv[i] > 1L) expect_true(r[1] > tau)      # and is the smallest such
  }
})

test_that("ls_check accepts a per-record threshold vector", {
  x <- data.frame(a = c("1", "1", "2", "2"), b = c("1", "1", "2", "2"),
                  stringsAsFactors = FALSE)
  # every class has size 2: safe at k = 2 everywhere, unsafe where k_i = 3
  expect_true(ls_check(x, c("a", "b"), k = 2)$ok)
  chk <- ls_check(x, c("a", "b"), k = c(3, 2, 2, 2))
  expect_false(chk$ok)
  expect_equal(chk$violators, 1L)
})

test_that("solvers honour a non-uniform threshold vector", {
  set.seed(9)
  x <- data.frame(a = rep(c("1", "2"), each = 6),
                  b = rep(c("1", "2", "3"), 4),
                  stringsAsFactors = FALSE)
  kvec <- c(rep(3, 6), rep(2, 6))
  for (fn in list(ls_greedy2, ls_hub)) {
    res <- fn(x, keyVars = c("a", "b"), k = kvec)
    if (is.null(res$xAnon)) next
    expect_true(ls_check(res$xAnon, c("a", "b"), k = kvec)$ok)
  }
  opt <- ls_optimal(x, keyVars = c("a", "b"), k = kvec)
  expect_true(ls_check(opt$xAnon, c("a", "b"), k = kvec)$ok)
})

test_that("a risk target is solved end to end and verified by sdcMicro", {
  skip_if_not_installed("sdcMicro")
  data(francdat, package = "sdcMicro", envir = environment())
  kv <- c(4, 5, 6)
  thr <- ls_target(francdat, kv, risk = 0.5)
  res <- ls_greedy2(francdat, keyVars = kv, k = thr)
  expect_true(ls_check(res$xAnon, kv, k = thr)$ok)
})

test_that("risk thresholds do not depend on how the keys are stored", {
  ## freqCalc() coerces key columns with as.numeric(), so non-numeric
  ## character labels silently become NA -- that is, wildcards -- and the
  ## weighted counts come out far too large. ls_check() recodes for exactly this
  ## reason; ls_target() did not, and computed risk thresholds from a file it
  ## believed to be much safer than it is (found 2026-09-07 on eusilcS, where the
  ## raw call reported fk in 18..2770 against the true 1..264).
  set.seed(4)
  n <- 60
  x <- data.frame(
    region = sample(c("north", "south", "east"), n, TRUE),
    sex    = sample(c("male", "female"), n, TRUE),
    band   = sample(c("young", "mid", "old"), n, TRUE),
    w      = runif(n, 1, 10), stringsAsFactors = FALSE)
  kv <- c("region", "sex", "band")
  y <- x; for (v in kv) y[[v]] <- as.integer(factor(y[[v]]))
  expect_equal(as.integer(ls_target(x, kv, risk = 0.1, w = "w")),
               as.integer(ls_target(y, kv, risk = 0.1, w = "w")))
})
