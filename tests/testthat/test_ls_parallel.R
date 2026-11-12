# Strata in localSuppression()/kAnon() computed in parallel (issue #349).
# Fork cluster on Linux/macOS, PSOCK cluster on Windows; the results must be
# identical to the serial ones.

library(sdcMicro)

is_windows <- .Platform$OS.type == "windows"
# functions sent to PSOCK workers must not drag the test environment along
global_fun <- function(f) {
  environment(f) <- globalenv()
  f
}
kv <- c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex")

strata_data <- function() {
  data("testdata", package = "sdcMicro")
  testdata$ageG <- cut(testdata$age, 5, labels = paste0("AG", 1:5))
  testdata
}

test_that("ls_lapply() runs serially for nc = 1 and passes further arguments", {
  f <- function(i, a) i * a
  expect_identical(sdcMicro:::ls_lapply(1:5, f, a = 2, nc = 1), lapply(1:5, f, a = 2))
  # a single element is never worth a cluster
  expect_identical(sdcMicro:::ls_lapply(list(3), f, a = 2, nc = 4), list(6))
  expect_identical(sdcMicro:::ls_lapply(list(), f, a = 2, nc = 4), list())
})

test_that("ls_lapply() with a fork cluster keeps the order of the results", {
  skip_if(is_windows, "no fork on Windows")
  f <- function(i, a) {
    Sys.sleep((6 - i) / 100)
    i * a
  }
  res <- sdcMicro:::ls_lapply(1:5, f, a = 2, nc = 2, cl_type = "FORK")
  expect_identical(res, lapply(1:5, function(i) i * 2))
  expect_error(sdcMicro:::ls_lapply(1:5, f, a = 2, nc = 2, cl_type = "fork"))
})

test_that("ls_lapply() with a PSOCK cluster keeps the order of the results", {
  skip_on_cran()
  f <- global_fun(function(i, a) {
    Sys.sleep((6 - i) / 100)
    i * a
  })
  res <- sdcMicro:::ls_lapply(1:5, f, a = 2, nc = 2, cl_type = "PSOCK")
  expect_identical(res, lapply(1:5, function(i) i * 2))
})

test_that("ls_lapply() workers use one data.table thread and one solver thread", {
  skip_on_cran()
  probe <- global_fun(function(i) {
    c(dt = data.table::getDTthreads(), milp = getOption("sdcMicro.milp_threads", NA))
  })
  if (!is_windows) {
    res <- sdcMicro:::ls_lapply(1:2, probe, nc = 2, cl_type = "FORK")
    expect_true(all(vapply(res, function(r) all(r == 1), logical(1))))
  }
  res <- sdcMicro:::ls_lapply(1:2, probe, nc = 2, cl_type = "PSOCK")
  expect_true(all(vapply(res, function(r) all(r == 1), logical(1))))
  # the calling session is left alone
  expect_null(getOption("sdcMicro.milp_threads"))
})

test_that("ls_lapply() passes on an error raised in a worker", {
  skip_on_cran()
  f <- global_fun(function(i) {
    if (i == 2) stop("stratum two failed")
    i
  })
  if (!is_windows) {
    expect_error(sdcMicro:::ls_lapply(1:3, f, nc = 2, cl_type = "FORK"), "stratum two failed")
  }
  expect_error(sdcMicro:::ls_lapply(1:3, f, nc = 2, cl_type = "PSOCK"), "stratum two failed")
})

test_that("the number of solver threads is read from an option", {
  old <- options(sdcMicro.milp_threads = NULL)
  on.exit(options(old), add = TRUE)
  expect_null(sdcMicro:::ls_milp_threads())
  options(sdcMicro.milp_threads = 2)
  expect_identical(sdcMicro:::ls_milp_threads(), 2L)
  options(sdcMicro.milp_threads = 0)
  expect_error(sdcMicro:::ls_milp_threads(), "sdcMicro.milp_threads")
})

test_that("kAnon() with strata gives identical results for nc = 1 and nc = 2", {
  skip_on_cran()
  skip_if(is_windows, "fork cluster")
  skip_if(parallel::detectCores() < 3, "needs at least 3 cores")
  x <- strata_data()
  for (m in c("heuristic", "greedy2")) {
    r1 <- kAnon(x, keyVars = kv, strataVars = "ageG", k = 3, method = m, nc = 1)
    r2 <- kAnon(x, keyVars = kv, strataVars = "ageG", k = 3, method = m, nc = 2)
    r1$time <- r2$time <- NULL
    expect_identical(r1, r2, info = m)
  }
})

test_that("kAnon() with strata and combs gives identical results for nc = 1 and nc = 2", {
  skip_on_cran()
  skip_if(is_windows, "fork cluster")
  skip_if(parallel::detectCores() < 3, "needs at least 3 cores")
  x <- strata_data()
  for (m in c("heuristic", "greedy2")) {
    r1 <- kAnon(x, keyVars = kv, strataVars = "ageG", k = c(3, 2), combs = c(6, 5), method = m, nc = 1)
    r2 <- kAnon(x, keyVars = kv, strataVars = "ageG", k = c(3, 2), combs = c(6, 5), method = m, nc = 2)
    r1$time <- r2$time <- NULL
    expect_identical(r1, r2, info = m)
  }
})

test_that("an exact engine gives the same suppressions for nc = 1 and nc = 2", {
  skip_on_cran()
  skip_if_not_installed("highs")
  skip_if(is_windows, "fork cluster")
  skip_if(parallel::detectCores() < 3, "needs at least 3 cores")
  x <- strata_data()
  kv4 <- kv[1:4]
  ctrl <- list(solver = "highs")
  r1 <- kAnon(x, keyVars = kv4, strataVars = "ageG", k = 3, method = "optimal", control = ctrl, nc = 1)
  r2 <- kAnon(x, keyVars = kv4, strataVars = "ageG", k = 3, method = "optimal", control = ctrl, nc = 2)
  expect_identical(r1$status, "optimal")
  expect_identical(r2$status, "optimal")
  expect_equal(r1$objective, r2$objective)
  expect_identical(r1$xAnon, r2$xAnon)
})

test_that("an error in one stratum stops kAnon() also when run in parallel", {
  skip_on_cran()
  skip_if(is_windows, "fork cluster")
  skip_if(parallel::detectCores() < 3, "needs at least 3 cores")
  x <- strata_data()[1:400, ]
  # one stratum with a single record: k = 3 cannot be reached there
  x$s <- rep(c("a", "b", "c"), length.out = 400)
  x$s[1] <- "single"
  msg <- "k is larger or equal the group size"
  expect_error(kAnon(x, keyVars = kv, strataVars = "s", k = 3, method = "greedy2", nc = 1), msg)
  expect_error(suppressWarnings(
    kAnon(x, keyVars = kv, strataVars = "s", k = 3, method = "greedy2", nc = 2)
  ), msg)
})

test_that("kAnon() with a PSOCK cluster gives the serial result", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 3, "needs at least 3 cores")
  # PSOCK workers load the *installed* sdcMicro; under devtools::load_all() this
  # is not the code under test
  # (devtools::load_all() marks the namespace with '.__DEVTOOLS__')
  is_dev <- exists(".__DEVTOOLS__", envir = asNamespace("sdcMicro"), inherits = FALSE)
  skip_if(is_dev, "PSOCK workers would load the installed package, not the source tree")
  x <- strata_data()
  old <- options(sdcMicro.ls_cluster_type = "PSOCK")
  on.exit(options(old), add = TRUE)
  for (m in c("heuristic", "greedy2")) {
    r1 <- kAnon(x, keyVars = kv, strataVars = "ageG", k = 3, method = m, nc = 1)
    r2 <- kAnon(x, keyVars = kv, strataVars = "ageG", k = 3, method = m, nc = 2)
    r1$time <- r2$time <- NULL
    expect_identical(r1, r2, info = m)
  }
  r1 <- kAnon(x, keyVars = kv, strataVars = "ageG", k = c(3, 2), combs = c(6, 5), nc = 1)
  r2 <- kAnon(x, keyVars = kv, strataVars = "ageG", k = c(3, 2), combs = c(6, 5), nc = 2)
  expect_identical(r1, r2)
})
