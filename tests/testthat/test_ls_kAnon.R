## localSuppression(method = ): the ls_* engines reached through sdcMicro's own
## entry point. Everything above suppSubset() -- strata, combs, ghost variables,
## the suppression accounting, the sdcMicroObj slots -- is shared code, so these
## tests pin that it all still travels through when the engine changes.

toy <- function() {
  ## 4 key variables, a stratum column, and one pre-existing NA
  set.seed(11)
  x <- data.frame(
    a = factor(rep(c("x", "y"), each = 20)),
    b = factor(rep(c("p", "q", "r", "s"), times = 10)),
    c = factor(sample(c("1", "2", "3"), 40, TRUE)),
    d = factor(sample(c("u", "v"), 40, TRUE)),
    str = factor(rep(c("A", "B"), times = 20))
  )
  x$c[3] <- NA
  x
}

# ---- ls_supp_subset(): the engine dispatcher -------------------------------

test_that("ls_supp_subset returns k-anonymous keys for every engine", {
  x <- toy()[, c("a", "b", "c", "d")]
  ## the control list is filtered per engine: greedy2 accepts neither entry
  for (m in c("greedy2", "optimal", "aggregate", "lns")) {
    res <- ls_supp_subset(x, k = 2, method = m,
                          control = list(time_limit = 5, iterations = 5))
    expect_named(res, c("xAnon", "info"))
    expect_identical(dim(res$xAnon), dim(x))
    expect_identical(names(res$xAnon), names(x))
    expect_true(ls_check(res$xAnon, keyVars = names(x), k = 2)$ok,
                info = paste("engine", m))
    ## never fewer NA than we started with
    expect_gte(sum(is.na(res$xAnon)), sum(is.na(x)))
  }
})

test_that("ls_supp_subset refuses what the solvers cannot honour", {
  x <- toy()[, c("a", "b", "c", "d")]
  expect_error(ls_supp_subset(x, k = 2, alpha = 0.5, method = "greedy2"), "alpha")
  expect_error(ls_supp_subset(x, k = c(2, 2), method = "greedy2"), "length 1")
  expect_error(ls_supp_subset(x, k = 2, method = "nonesuch"))
  ## the aggregated model is exact for every alpha and is not refused
  res <- ls_supp_subset(x, k = 2, alpha = 0.5, method = "aggregate")
  expect_true(ls_check(res$xAnon, keyVars = names(x), k = 2, alpha = 0.5)$ok)
})

test_that("ls_supp_subset passes importance through as a cost, not an order", {
  x <- toy()[, c("a", "b", "c", "d")]
  ## make "a" by far the most important: it should be suppressed least
  cheap <- ls_supp_subset(x, k = 3, importance = c(1, 4, 4, 4), method = "optimal")
  expect_lte(sum(is.na(cheap$xAnon$a)), sum(is.na(x$a)) + 1L)
})

# ---- localSuppression(method = ) on a data.frame: strata, combs, counting ----

test_that("without strata or combs localSuppression() equals a direct engine call", {
  x <- toy()
  kv <- c("a", "b", "c", "d")
  direct <- ls_greedy2(x[, kv], keyVars = kv, k = 2)
  via <- localSuppression(x, keyVars = kv, k = 2, method = "greedy2")
  expect_equal(sum(is.na(via$xAnon[, kv])), sum(is.na(direct$xAnon)))
})

test_that("suppression counts have sdcMicro's shape and exclude pre-existing NA", {
  x <- toy()
  kv <- c("a", "b", "c", "d")
  res <- localSuppression(x, keyVars = kv, k = 2, method = "greedy2")

  expect_s3_class(res, "localSuppression")
  expect_identical(colnames(res$supps), kv)
  expect_identical(colnames(res$totalSupps), kv)
  expect_identical(nrow(res$supps), 1L)
  ## totalSupps counts the NA that was already there; supps does not
  expect_equal(res$totalSupps$c, sum(is.na(res$xAnon$c)))
  expect_equal(res$supps$c, sum(is.na(res$xAnon$c)) - 1L)
  expect_equal(res$newSupps, unname(utils::tail(rowSums(res$supps), 1)))
  ## the fields the GUI and the report need
  expect_true(all(c("method", "objective", "time") %in% names(res)))
})

test_that("combs makes every subset of that size k-anonymous", {
  x <- toy()
  kv <- c("a", "b", "c", "d")
  res <- localSuppression(x, keyVars = kv, k = 2, combs = 3, method = "greedy2")
  for (sub in utils::combn(kv, 3, simplify = FALSE)) {
    expect_true(ls_check(res$xAnon, keyVars = sub, k = 2)$ok,
                info = paste(sub, collapse = "+"))
  }
})

test_that("strataVars protects each stratum separately", {
  x <- toy()
  kv <- c("a", "b", "c", "d")
  res <- localSuppression(x, keyVars = kv, strataVars = "str", k = 2,
                             method = "greedy2")
  ## sdcMicro's xAnon carries the key columns only; put the stratum back to check
  d <- res$xAnon; d$str <- x$str
  expect_true(ls_check(d, keyVars = kv, k = 2, strataVars = "str")$ok)
  ## one row per stratum plus a Total row, as sdcMicro does it
  expect_identical(rownames(res$totalSupps), c("A", "B", "Total"))
})

test_that("the result prints through sdcMicro's own print method", {
  res <- localSuppression(toy(), keyVars = c("a", "b", "c", "d"), k = 2,
                             method = "greedy2")
  expect_message(print(res), "Total number of suppressions")
  expect_message(print(res), "Method: greedy2")
})

# ---- kAnon(method = ) on an sdcMicroObj -------------------------------------

test_that("kAnon(method = \"greedy2\") protects an sdcMicroObj and never costs more than the sweep", {
  data(testdata2, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water", "sex")
  sdc <- createSdcObj(testdata2, keyVars = kv, w = "sampling_weight")

  ours <- kAnon(sdc, k = 2, method = "greedy2")
  theirs <- kAnon(sdc, k = 2)

  expect_s4_class(ours, "sdcMicroObj")
  km <- as.data.frame(get.sdcMicroObj(ours, "manipKeyVars"))
  expect_true(ls_check(km, keyVars = kv, k = 2)$ok)
  expect_lte(sum(is.na(km)),
             sum(is.na(as.data.frame(get.sdcMicroObj(theirs, "manipKeyVars")))))
  ## the slot sdcApp reads
  expect_false(is.null(get.sdcMicroObj(ours, "localSuppression")$supps))
})

test_that("kAnon(method = ) transfers the suppression pattern to ghost variables", {
  data(testdata2, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water", "sex")
  sdc <- createSdcObj(testdata2, keyVars = kv, w = "sampling_weight",
                      ghostVars = list(list("urbrur", "expend")))
  ours <- kAnon(sdc, k = 3, method = "greedy2")

  km <- as.data.frame(get.sdcMicroObj(ours, "manipKeyVars"))
  gv <- get.sdcMicroObj(ours, "manipGhostVars")
  expect_true(sum(is.na(km$urbrur)) > 0)
  expect_identical(is.na(gv[["expend"]]), is.na(km$urbrur))
})

test_that("kAnon(method = ) refuses an sdcMicroObj whose alpha the solvers cannot honour", {
  data(testdata2, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water")
  sdc <- createSdcObj(testdata2, keyVars = kv, w = "sampling_weight", alpha = 0.5)
  expect_error(kAnon(sdc, k = 2, method = "greedy2"), "alpha")
})

test_that("kAnon(method = ) honours the object's strataVar slot", {
  data(testdata, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water")
  sdc <- createSdcObj(testdata, keyVars = kv, w = "sampling_weight")
  strataVar(sdc) <- "sex"

  ours <- kAnon(sdc, k = 3, method = "greedy2")
  ls <- get.sdcMicroObj(ours, "localSuppression")
  expect_identical(ls$strataVars, "sex")
  expect_identical(rownames(ls$totalSupps), c("1", "2", "Total"))

  km <- as.data.frame(get.sdcMicroObj(ours, "manipKeyVars"))
  km$sex <- testdata$sex
  expect_true(ls_check(km, keyVars = kv, k = 3, strataVars = "sex")$ok)
})

test_that("kAnon(method = ) honours combs through the object path", {
  data(testdata, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water")
  sdc <- createSdcObj(testdata, keyVars = kv, w = "sampling_weight")

  ours <- kAnon(sdc, k = 2, combs = 3, method = "greedy2")
  km <- as.data.frame(get.sdcMicroObj(ours, "manipKeyVars"))
  for (sub in utils::combn(kv, 3, simplify = FALSE)) {
    expect_true(ls_check(km, keyVars = sub, k = 2)$ok,
                info = paste(sub, collapse = "+"))
  }
  ## and no more cells than sdcMicro needs for the same requirement
  theirs <- kAnon(sdc, k = 2, combs = 3)
  expect_lte(sum(is.na(km)),
             sum(is.na(as.data.frame(get.sdcMicroObj(theirs, "manipKeyVars")))))
})


# ---- the acceptance test: method = "heuristic" is the 5.8.x sweep, unchanged ----

test_that("the default method reproduces the original sweep cell for cell", {
  data(testdata, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water", "sex")
  a <- localSuppression(testdata, keyVars = kv, k = 3)
  b <- localSuppression(testdata, keyVars = kv, k = 3, method = "heuristic")
  expect_identical(a$xAnon, b$xAnon)
  expect_identical(a$supps, b$supps)
  expect_identical(a$method, "heuristic")
  expect_null(a$objective)
  ## and with strata and combs, the two paths the engines never see
  s1 <- localSuppression(testdata, keyVars = kv[1:4], strataVars = "sex", k = 2, combs = 3)
  s2 <- localSuppression(testdata, keyVars = kv[1:4], strataVars = "sex", k = 2, combs = 3,
                         method = "heuristic")
  expect_identical(s1$xAnon, s2$xAnon)
})

test_that("a default importance order is never handed to an engine as a cost", {
  ## the sweep materialises an importance from the category counts; the
  ## engines must see NULL (unit costs) unless the user supplied one
  data(testdata, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water")
  via <- localSuppression(testdata, keyVars = kv, k = 3, method = "greedy2")
  direct <- ls_greedy2(testdata, keyVars = kv, k = 3)
  expect_equal(sum(is.na(via$xAnon[, kv])), sum(is.na(direct$xAnon[, kv])))
  expect_equal(via$objective, direct$objective)
  ## a user-supplied importance does reach the engine, as a cost
  imp <- localSuppression(testdata, keyVars = kv, k = 3, importance = c(1, 2, 3, 4),
                          method = "greedy2")
  expect_identical(imp$importance, c(1, 2, 3, 4))
})
