# sdcMicroObj without categorical key variables (issue #91): only numerical key
# variables are defined. Methods for numerical variables work, methods that need
# categorical key variables stop with a clear message.

library(sdcMicro)

num_vars <- c("CURRENT.ASSETS", "SALES", "DEPRECIATION", "GROSS.PROFIT", "NET.PROFIT")
no_kv <- "needs categorical key variables"

tarragona <- function() {
  data("Tarragona", package = "sdcMicro")
  set.seed(91)
  Tarragona$w <- runif(nrow(Tarragona), 1, 5)
  Tarragona$grp <- sample(c("a", "b", "c"), nrow(Tarragona), replace = TRUE)
  Tarragona$cat1 <- factor(sample(1:4, nrow(Tarragona), replace = TRUE))
  Tarragona
}
num_only <- function(...) {
  createSdcObj(tarragona(), numVars = num_vars, ...)
}
manip_num <- function(obj) {
  get.sdcMicroObj(obj, type = "manipNumVars")
}

test_that("createSdcObj() works with numerical key variables only", {
  dat <- tarragona()
  sdc <- createSdcObj(dat, numVars = num_vars)
  expect_s4_class(sdc, "sdcMicroObj")
  expect_null(get.sdcMicroObj(sdc, type = "keyVars"))
  expect_null(get.sdcMicroObj(sdc, type = "manipKeyVars"))
  expect_identical(get.sdcMicroObj(sdc, type = "numVars"), match(num_vars, colnames(dat)))
  expect_equal(manip_num(sdc), dat[, num_vars], ignore_attr = TRUE)

  # no risk for categorical key variables, the measures for numerical ones are there
  risk <- get.sdcMicroObj(sdc, type = "risk")
  expect_null(risk$individual)
  expect_null(risk$global)
  expect_true(is.numeric(risk$numeric) && length(risk$numeric) == 1)
  expect_null(get.sdcMicroObj(sdc, type = "originalRisk")$individual)
  expect_equal(get.sdcMicroObj(sdc, type = "utility")$il1, 0)

  # keyVars = NULL is the same as leaving it out; indices work as well
  expect_identical(createSdcObj(dat, keyVars = NULL, numVars = num_vars), sdc)
  expect_identical(createSdcObj(dat, numVars = match(num_vars, colnames(dat))), sdc)
})

test_that("createSdcObj() needs categorical or numerical key variables", {
  dat <- tarragona()
  expect_error(createSdcObj(dat), "'keyVars'.*'numVars'")
  expect_error(createSdcObj(dat, weightVar = "w"), "'keyVars'.*'numVars'")
  expect_error(
    createSdcObj(dat, numVars = num_vars, ghostVars = list(list("cat1", "grp"))),
    "ghostVars"
  )
})

test_that("keyVars is still the second positional argument of createSdcObj()", {
  dat <- tarragona()
  a <- createSdcObj(dat, "cat1", num_vars)
  b <- createSdcObj(dat, keyVars = "cat1", numVars = num_vars)
  expect_identical(a, b)
  expect_identical(colnames(get.sdcMicroObj(a, type = "manipKeyVars")), "cat1")
  expect_false(is.null(get.sdcMicroObj(a, type = "risk")$individual))
})

# the print methods write with message()
printed <- function(expr) {
  paste(testthat::capture_messages(expr), collapse = "\n")
}

test_that("an object without categorical key variables can be printed", {
  sdc <- num_only()
  out <- printed(show(sdc))
  expect_match(out, "Numerical key variables: CURRENT.ASSETS")
  expect_false(grepl("Anonymity", out))
  expect_false(grepl("ategorical key variables", out))

  # sections on categorical key variables say that there are none
  for (ty in c("kAnon", "risk", "recode")) {
    expect_match(printed(print(sdc, type = ty)), "No categorical key variables", info = ty)
    expect_null(print(sdc, type = ty, docat = FALSE))
  }
  expect_match(printed(print(sdc)), "No categorical key variables")
  for (ty in c("general", "numrisk", "comp_numvars", "ls", "pram")) {
    expect_error(printed(print(sdc, type = ty)), NA, info = ty)
  }
  expect_match(printed(print(microaggregation(sdc), type = "numrisk")), "Disclosure risk")
})

test_that("methods for numerical variables work without categorical key variables", {
  sdc <- num_only()
  orig <- manip_num(sdc)
  changed <- function(obj, vars = num_vars) {
    m <- manip_num(obj)
    expect_identical(dim(m), dim(orig))
    risk <- get.sdcMicroObj(obj, type = "risk")
    expect_true(risk$numeric >= 0 && risk$numeric <= 1)
    expect_null(risk$individual)
    all(vapply(vars, function(v) !isTRUE(all.equal(m[[v]], orig[[v]])), logical(1)))
  }
  set.seed(91)
  expect_true(changed(microaggregation(sdc)))
  expect_true(changed(microaggregation(sdc, method = "rmd", aggr = 4)))
  expect_true(changed(mafast(sdc, aggr = 3)))
  expect_true(changed(addNoise(sdc)))
  expect_true(changed(addNoise(sdc, method = "ROMM", p = 0.05)))
  expect_true(changed(rankSwap(sdc)))
  expect_true(changed(
    shuffle(sdc, form = SALES + NET.PROFIT ~ CURRENT.ASSETS + DEPRECIATION),
    vars = c("SALES", "NET.PROFIT")
  ))
  expect_true(changed(
    topBotCoding(sdc, value = 1e5, replacement = 1e5, kind = "top", column = "SALES"),
    vars = "SALES"
  ))
  expect_true(changed(microaggrGower(sdc, aggr = 3)))

  ma <- microaggregation(sdc)
  expect_gt(get.sdcMicroObj(ma, type = "utility")$il1, 0)
  expect_equal(manip_num(undolast(ma)), orig)
  expect_identical(get.sdcMicroObj(calcRisks(ma), type = "risk"), get.sdcMicroObj(ma, type = "risk"))
  expect_error(dRisk(ma), NA)
  expect_error(dUtility(ma), NA)
  expect_error(dRiskRMD(ma), NA)
})

test_that("weights and strata can be used without categorical key variables", {
  sdc <- num_only(weightVar = "w", strataVar = "grp")
  expect_identical(colnames(tarragona())[get.sdcMicroObj(sdc, type = "weightVar")], "w")
  ma <- microaggregation(sdc, method = "mdav", aggr = 3, strata_variables = "grp")
  m <- manip_num(ma)
  # every value is shared by at least three records of the same stratum
  n_same <- ave(m$SALES, tarragona()$grp, m$SALES, FUN = length)
  expect_true(all(n_same >= 3))
  expect_error(mafast(sdc, by = "grp", aggr = 3), NA)
})

test_that("the manipulated data can be extracted and written", {
  dat <- tarragona()
  ma <- microaggregation(num_only())
  res <- extractManipData(ma)
  expect_identical(dim(res), dim(dat))
  expect_identical(colnames(res), colnames(dat))
  expect_equal(res[, num_vars], manip_num(ma), ignore_attr = TRUE)
  other <- setdiff(colnames(dat), num_vars)
  expect_equal(res[, other], dat[, other], ignore_attr = TRUE)

  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  writeSafeFile(ma, format = "csv", randomizeRecords = "no", fileOut = f)
  expect_true(file.exists(f))
})

test_that("methods for categorical key variables stop with a clear message", {
  sdc <- num_only()
  expect_error(localSuppression(sdc), no_kv)
  expect_error(kAnon(sdc), no_kv)
  expect_error(localSupp(sdc, threshold = 0.1, keyVar = "cat1"), no_kv)
  expect_error(globalRecode(sdc, column = "cat1", breaks = 2), no_kv)
  expect_error(groupAndRename(sdc, var = "cat1", before = c("1", "2"), after = c("12", "12")), no_kv)
  expect_error(varToFactor(sdc, var = "SALES"), no_kv)
  expect_error(varToNumeric(sdc, var = "cat1"), no_kv)
  expect_error(ldiversity(sdc, ldiv_index = "cat1"), no_kv)
  expect_error(LocalRecProg(sdc), no_kv)
  expect_error(suda2(sdc), no_kv)
  expect_error(riskyCells(sdc, useIdentificationLevel = FALSE, threshold = 3, maxDim = 2), no_kv)
  expect_error(modRisk(sdc, method = "default", formulaM = ~ cat1), no_kv)
  expect_error(measure_risk(sdc), no_kv)
  expect_error(freq(sdc), no_kv)
  expect_error(kAnon_violations(sdc, FALSE, 3), no_kv)
  expect_error(addGhostVars(sdc, keyVar = "cat1", ghostVars = "grp"), no_kv)
  # stops before any request is sent to a language model
  expect_error(AI_applyAnonymization(sdc), no_kv)
  # the message names the function
  expect_error(kAnon(sdc), "kAnon()", fixed = TRUE)
  expect_error(suda2(sdc), "suda2()", fixed = TRUE)
})

test_that("pram() works without categorical key variables", {
  dat <- tarragona()
  set.seed(91)
  res <- pram(num_only(pramVars = "cat1"))
  expect_false(is.null(get.sdcMicroObj(res, type = "pram")))
  expect_null(get.sdcMicroObj(res, type = "risk")$individual)
  expect_false(identical(extractManipData(res)$cat1, dat$cat1))

  # as with key variables: a variable given in 'variables' becomes a pram variable
  res <- pram(num_only(), variables = "cat1")
  expect_identical(get.sdcMicroObj(res, type = "pramVars"), match("cat1", colnames(dat)))
  expect_error(pram(num_only()), "pramVars")
})

test_that("report() runs for an object without categorical key variables", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  ma <- microaggregation(num_only())
  out <- tempfile("report91")
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  for (internal in c(FALSE, TRUE)) {
    fn <- paste0("report_", internal)
    expect_error(
      suppressWarnings(report(ma, outdir = out, filename = fn, internal = internal, verbose = FALSE)),
      NA
    )
    expect_true(file.exists(file.path(out, paste0(fn, ".html"))))
  }
})
