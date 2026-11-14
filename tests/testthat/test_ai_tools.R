test_that("execute_tool_calls applies groupAndRename correctly", {
  data(testdata2, package = "sdcMicro")
  testdata2$roof <- as.factor(testdata2$roof)
  sdc <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
  calls <- list(
    list(tool = "groupAndRename", var = "roof",
         before = c("2", "4", "5", "6", "9"),
         after  = c("2", "4", "5", "5", "9"))
  )
  result <- execute_tool_calls(sdc, calls)
  anon_levels <- levels(result@manipKeyVars[["roof"]])
  expect_false("6" %in% anon_levels)
})

test_that("execute_tool_calls applies localSuppression", {
  data(testdata2, package = "sdcMicro")
  sdc <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
  calls <- list(
    list(tool = "localSuppression", k = 3)
  )
  result <- execute_tool_calls(sdc, calls)
  fk <- result@risk$individual[, "fk"]
  expect_true(all(fk >= 3))
})

test_that("execute_tool_calls applies microaggregation", {
  data(testdata2, package = "sdcMicro")
  sdc <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
  calls <- list(
    list(tool = "microaggregation", variables = c("income"), method = "mdav")
  )
  result <- execute_tool_calls(sdc, calls)
  orig_income <- get.sdcMicroObj(sdc, type = "origData")[["income"]]
  anon_income <- result@manipNumVars[["income"]]
  expect_false(identical(orig_income, anon_income))
})

test_that("execute_tool_calls rejects invalid variable names", {
  data(testdata2, package = "sdcMicro")
  sdc <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
  calls <- list(
    list(tool = "groupAndRename", var = "NONEXISTENT",
         before = c("a"), after = c("b"))
  )
  expect_error(execute_tool_calls(sdc, calls), "not found")
})

test_that("execute_tool_calls rejects unknown tools", {
  data(testdata2, package = "sdcMicro")
  sdc <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof"),
    w = "sampling_weight")
  calls <- list(
    list(tool = "dropTable")
  )
  expect_error(execute_tool_calls(sdc, calls), "Unknown tool")
})

test_that("get_tool_schemas returns valid list", {
  schemas <- get_tool_schemas()
  expect_type(schemas, "list")
  expect_true(length(schemas) >= 5)
  tool_names <- vapply(schemas, function(s) s$name, character(1))
  expect_true("groupAndRename" %in% tool_names)
  expect_true("localSuppression" %in% tool_names)
  expect_true("microaggregation" %in% tool_names)
})

test_that("execute_tool_calls applies onedimsopt and the refinement step where available", {
  data(testdata2, package = "sdcMicro")
  mk <- function() createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
  num <- c("expend", "income", "savings")
  r <- execute_tool_calls(mk(), list(list(tool = "microaggregation", variables = "income",
    method = "onedimsopt")))
  expect_equal(r@manipNumVars$income,
    microaggregation(testdata2[, "income", drop = FALSE], method = "onedimsopt")$mx$income)

  r <- execute_tool_calls(mk(), list(list(tool = "microaggregation", variables = num,
    method = "mdav", refine = TRUE)))
  expect_equal(r@manipNumVars, microaggregation(mk(), method = "mdav", refine = TRUE)@manipNumVars)
  expect_false(isTRUE(all.equal(r@manipNumVars, microaggregation(mk(), method = "mdav")@manipNumVars)))

  # refine is ignored where microaggregation() does not offer it
  r <- execute_tool_calls(mk(), list(list(tool = "microaggregation", variables = num,
    method = "onedimsopt", refine = TRUE)))
  expect_equal(r@manipNumVars, microaggregation(mk(), method = "onedimsopt")@manipNumVars)
  r <- execute_tool_calls(mk(), list(list(tool = "microaggregation", variables = "income",
    method = "mdav", refine = TRUE)))
  expect_equal(r@manipNumVars, microaggregation(mk(), variables = "income", method = "mdav")@manipNumVars)

  props <- Filter(function(s) s$name == "microaggregation", get_tool_schemas())[[1]]$parameters
  expect_true("refine" %in% names(props))
  expect_match(props$method$description, "onedimsopt")
})
