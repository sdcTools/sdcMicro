# sdcApp offers optimal univariate microaggregation (method "onedimsopt") and the
# refinement step (refine = TRUE) on the microaggregation page.
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
  createSdcObj(Tarragona[1:300, ], numVars = num_vars)
}

test_that("the page offers onedimsopt without aggregation statistic and without refinement", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    session$setInputs(rb_microagg_cluster = "No", sel_microagg_strata = "no stratification")
    expect_match(html(output$ui_microagg_method), "<option value=\"onedimsopt\"", fixed = TRUE)
    expect_match(html(output$ui_microagg_method), "optimal univariate microaggregation")

    session$setInputs(sel_microagg_method = "onedimsopt")
    page <- html(output$ui_microaggregation)
    expect_false(grepl("ui_microagg_aggmeasures|ui_microagg_trim|ui_microagg_refine|ui_microagg_gamma", page))
    expect_false(microagg_refine_ok())
    session$setInputs(sel_microagg_method = "onedims")
    page <- html(output$ui_microaggregation)
    expect_false(grepl("ui_microagg_refine", page))
    expect_match(page, "ui_microagg_aggmeasures", fixed = TRUE)

    # cluster-based methods do not list it
    session$setInputs(rb_microagg_cluster = "Yes")
    expect_false(grepl("onedimsopt", html(output$ui_microagg_method)))
  }))
})

test_that("the refinement is offered for the multivariate methods, default No", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    session$setInputs(rb_microagg_cluster = "No", sel_microagg_strata = "no stratification")
    for (m in c("mdav", "vmdav", "rmd", "pca")) {
      session$setInputs(sel_microagg_method = m)
      expect_match(html(output$ui_microaggregation), "ui_microagg_refine", fixed = TRUE)
      expect_true(microagg_refine_ok())
    }
    rb <- html(output$ui_microagg_refine)
    expect_match(rb, "id=\"rb_microagg_refine\"", fixed = TRUE)
    expect_match(rb, "value=\"No\" checked", fixed = TRUE)

    # a single variable: only a hint, except with vmdav
    session$setInputs(sel_microagg_method = "mdav", sel_microagg_v = "SALES")
    expect_false(microagg_refine_ok())
    expect_false(grepl("rb_microagg_refine", html(output$ui_microagg_refine)))
    expect_match(html(output$ui_microagg_refine), "not available for a single variable")
    session$setInputs(sel_microagg_method = "vmdav")
    expect_true(microagg_refine_ok())
    expect_match(html(output$ui_microagg_refine), "rb_microagg_refine", fixed = TRUE)
  }))
})

test_that("the generated call carries refine only where it is chosen and available", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    session$setInputs(rb_microagg_cluster = "No", sel_microagg_strata = "no stratification",
      sel_microagg_method = "mdav", sl_microagg_aggr = 4)
    expect_identical(code_microaggregation()$cmd,
      "sdcObj <- microaggregation(obj=sdcObj, variables=NULL, aggr=4, method=\"mdav\")")
    session$setInputs(rb_microagg_refine = "Yes")
    res <- code_microaggregation()
    expect_identical(res$cmd,
      "sdcObj <- microaggregation(obj=sdcObj, variables=NULL, aggr=4, method=\"mdav\", refine=TRUE)")
    expect_match(res$txt_action, "(method=\"mdav\" and size=4, groups refined by a local search)",
      fixed = TRUE)

    session$setInputs(sel_microagg_method = "vmdav", sl_microagg_gamma = 1.1)
    res <- code_microaggregation()
    expect_identical(res$cmd,
      "sdcObj <- microaggregation(obj=sdcObj, variables=NULL, aggr=4, method=\"vmdav\", gamma=1.1, refine=TRUE)")
    expect_match(res$txt_action, "gamma=1.1, groups refined by a local search)", fixed = TRUE)

    session$setInputs(sel_microagg_method = "pca", sl_microagg_measure = "mean", sl_microagg_trim = 0)
    expect_identical(code_microaggregation()$cmd, paste0("sdcObj <- microaggregation(obj=sdcObj, ",
      "variables=NULL, aggr=4, method=\"pca\", measure=\"mean\", trim=0, varsort=1, refine=TRUE)"))

    # 'Yes' is still set, but the choice is not available: no refine in the call
    session$setInputs(sel_microagg_method = "onedimsopt")
    res <- code_microaggregation()
    expect_identical(res$cmd,
      "sdcObj <- microaggregation(obj=sdcObj, variables=NULL, aggr=4, method=\"onedimsopt\")")
    expect_match(res$txt_action, "(method=\"onedimsopt\" and size=4)", fixed = TRUE)
    session$setInputs(sel_microagg_method = "mdav", sel_microagg_v = "SALES")
    expect_false(grepl("refine", code_microaggregation()$cmd))
  }))
})

test_that("the button runs onedimsopt and the refinement, and the script reproduces them", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    session$setInputs(rb_microagg_cluster = "No", sel_microagg_strata = "no stratification",
      sel_microagg_method = "mdav", sl_microagg_aggr = 3, rb_microagg_refine = "Yes")
    session$setInputs(btn_microagg = 1)
    expect_null(lastError())
    ref <- microaggregation(num_sdc(), method = "mdav", aggr = 3, refine = TRUE)
    expect_equal(get.sdcMicroObj(obj$sdcObj, "manipNumVars"), get.sdcMicroObj(ref, "manipNumVars"))
    expect_false(isTRUE(all.equal(get.sdcMicroObj(obj$sdcObj, "manipNumVars"),
      get.sdcMicroObj(microaggregation(num_sdc(), method = "mdav", aggr = 3), "manipNumVars"))))
    expect_match(obj$lastaction, "groups refined by a local search", fixed = TRUE)
    expect_true(any(grepl("method=\"mdav\", refine=TRUE)", obj$code_anonymize, fixed = TRUE)))
  }))
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    session$setInputs(rb_microagg_cluster = "No", sel_microagg_strata = "no stratification",
      sel_microagg_method = "onedimsopt", sl_microagg_aggr = 3)
    session$setInputs(btn_microagg = 1)
    expect_null(lastError())
    ref <- microaggregation(num_sdc(), method = "onedimsopt", aggr = 3)
    expect_equal(get.sdcMicroObj(obj$sdcObj, "manipNumVars"), get.sdcMicroObj(ref, "manipNumVars"))
    x <- get.sdcMicroObj(obj$sdcObj, "manipNumVars")
    expect_true(all(vapply(x, function(v) all(table(v) >= 3), logical(1))))
    expect_true(any(grepl("method=\"onedimsopt\")", obj$code_anonymize, fixed = TRUE)))
  }))
})
