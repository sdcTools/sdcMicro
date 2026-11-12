# sdcApp offers microaggregation with variable group size (method "vmdav", issue #311).
# The server is driven with shiny::testServer(); nothing is rendered in a browser.

skip_app <- function() {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not(utils::packageVersion("shiny") >= "1.5.0", "shiny::testServer needs shiny >= 1.5.0")
}
app_dir <- function() system.file("shiny", "sdcApp", package = "sdcMicro")
html <- function(x) paste(as.character(x), collapse = "")
# see test_sdcApp_numVarsOnly.R: two risk sliders of pages that are not shown warn
quiet_sliders <- function(code) {
  withCallingHandlers(code, warning = function(w) {
    if (grepl("returning -Inf|less than or equal to `max`", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}
num_vars <- c("CURRENT.ASSETS", "SALES", "DEPRECIATION", "GROSS.PROFIT", "NET.PROFIT")
num_sdc <- function() {
  data("Tarragona", package = "sdcMicro")
  createSdcObj(Tarragona, numVars = num_vars)
}
group_sizes <- function(sdc) {
  x <- get.sdcMicroObj(sdc, "manipNumVars")
  as.integer(table(do.call(paste, c(as.data.frame(x), sep = "\r"))))
}

test_that("the microaggregation page offers vmdav and shows the gain factor only for it", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    session$setInputs(rb_microagg_cluster = "No", sel_microagg_strata = "no stratification")
    expect_match(html(output$ui_microagg_method), "<option value=\"vmdav\"", fixed = TRUE)
    expect_match(html(output$ui_microagg_method), "variable group size")

    session$setInputs(sel_microagg_method = "mdav")
    expect_false(grepl("ui_microagg_gamma", html(output$ui_microaggregation)))
    session$setInputs(sel_microagg_method = "vmdav")
    page <- html(output$ui_microaggregation)
    expect_match(page, "ui_microagg_gamma", fixed = TRUE)
    # as for mdav and rmd: no aggregation statistic, no trimming
    expect_false(grepl("ui_microagg_aggmeasures|ui_microagg_trim", page))
    sl <- html(output$ui_microagg_gamma)
    expect_match(sl, "id=\"sl_microagg_gamma\"", fixed = TRUE)
    expect_match(sl, "data-from=\"0.2\"", fixed = TRUE)
    expect_match(sl, "data-min=\"0\"", fixed = TRUE)
    expect_match(html(output$ui_microagg_agglevel), "minimum group size")

    # cluster-based methods do not list it
    session$setInputs(rb_microagg_cluster = "Yes")
    expect_false(grepl("vmdav", html(output$ui_microagg_method)))
  }))
})

test_that("the generated call carries gamma for vmdav only", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    session$setInputs(rb_microagg_cluster = "No", sel_microagg_strata = "no stratification",
      sel_microagg_method = "vmdav", sl_microagg_aggr = 4, sl_microagg_gamma = 1.1)
    res <- code_microaggregation()
    expect_identical(res$cmd,
      "sdcObj <- microaggregation(obj=sdcObj, variables=NULL, aggr=4, method=\"vmdav\", gamma=1.1)")
    expect_match(res$txt_action, "method=\"vmdav\", minimum size=4 and gamma=1.1", fixed = TRUE)

    session$setInputs(sel_microagg_method = "mdav")
    res <- code_microaggregation()
    expect_identical(res$cmd,
      "sdcObj <- microaggregation(obj=sdcObj, variables=NULL, aggr=4, method=\"mdav\")")
    expect_match(res$txt_action, "(method=\"mdav\" and size=4)", fixed = TRUE)

    # the slider has not been rendered yet: default of the function
    session$setInputs(sel_microagg_method = "vmdav", sl_microagg_gamma = NULL)
    expect_match(code_microaggregation()$cmd, "gamma=0.2)", fixed = TRUE)
  }))
})

test_that("the button runs vmdav and the script reproduces it", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    session$setInputs(rb_microagg_cluster = "No", sel_microagg_strata = "no stratification",
      sel_microagg_method = "vmdav", sl_microagg_aggr = 3, sl_microagg_gamma = 1.1)
    session$setInputs(btn_microagg = 1)
    expect_null(lastError())
    ref <- microaggregation(num_sdc(), method = "vmdav", aggr = 3, gamma = 1.1)
    expect_equal(get.sdcMicroObj(obj$sdcObj, "manipNumVars"), get.sdcMicroObj(ref, "manipNumVars"))
    gs <- group_sizes(obj$sdcObj)
    expect_true(all(gs >= 3))
    expect_gt(length(unique(gs)), 1) # variable group size
    expect_match(obj$lastaction, "gamma=1.1", fixed = TRUE)
    expect_true(any(grepl("method=\"vmdav\", gamma=1.1)", obj$code_anonymize, fixed = TRUE)))
  }))
})

test_that("vmdav runs within strata in the app", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    data("testdata2", package = "sdcMicro")
    obj$sdcObj <- createSdcObj(testdata2, keyVars = c("urbrur", "roof", "walls"),
      numVars = c("expend", "income", "savings"), w = "sampling_weight")
    session$setInputs(rb_microagg_cluster = "No", sel_microagg_strata = "sex",
      sel_microagg_method = "vmdav", sl_microagg_aggr = 3, sl_microagg_gamma = 0.2)
    session$setInputs(btn_microagg = 1)
    expect_null(lastError())
    x <- get.sdcMicroObj(obj$sdcObj, "manipNumVars")
    key <- paste(testdata2$sex, do.call(paste, c(as.data.frame(x), sep = "\r")))
    expect_true(all(table(key) >= 3))
    # groups never mix strata
    expect_true(all(tapply(testdata2$sex, do.call(paste, c(as.data.frame(x), sep = "\r")),
      function(s) length(unique(s))) == 1))
  }))
})
