# sdcApp with an SDC problem that has numerical key variables only (issue #91).
# The server is driven with shiny::testServer(); nothing is rendered in a browser.

skip_app <- function() {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not(utils::packageVersion("shiny") >= "1.5.0", "shiny::testServer needs shiny >= 1.5.0")
}
app_dir <- function() system.file("shiny", "sdcApp", package = "sdcMicro")
html <- function(x) paste(as.character(x), collapse = "")
# testServer() evaluates every output, also the two risk sliders of pages that are
# not shown; without individual risks (no SDC problem yet, or no categorical key
# variables) they warn about max = -Inf. Only these two warnings are muffled.
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
cat_sdc <- function() {
  data("testdata2", package = "sdcMicro")
  createSdcObj(testdata2, keyVars = c("urbrur", "roof", "walls"),
               numVars = c("expend", "income"), w = "sampling_weight")
}
notice <- "No categorical key variables"

test_that("the app sets up a problem with numerical key variables only", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    data("Tarragona", package = "sdcMicro")
    obj$inputdata <- Tarragona
    session$flushReact() # new data: the ids of the setup inputs get a new counter
    vv <- obj$setupval_inc
    vars <- colnames(Tarragona)
    inp <- list(sl_seed = 1, sl_alpha = 1)
    for (i in seq_along(vars)) {
      inp[[paste0("setup_key_", vv, "_", i)]] <- if (vars[i] %in% num_vars) "Cont." else "No"
      inp[[paste0("setup_pram_", vv, "_", i)]] <- FALSE
      inp[[paste0("setup_weight_", vv, "_", i)]] <- FALSE
      inp[[paste0("setup_cluster_", vv, "_", i)]] <- FALSE
      inp[[paste0("setup_remove_", vv, "_", i)]] <- FALSE
    }
    do.call(session$setInputs, inp)

    cmd <- code_createSdcObj()$setup_sdc
    expect_match(cmd, "keyVars=NULL", fixed = TRUE)
    expect_match(cmd, "numVars=c(\"CURRENT.ASSETS\"", fixed = TRUE)

    session$setInputs(btn_setup_sdc = 1)
    expect_s4_class(obj$sdcObj, "sdcMicroObj")
    expect_false(has_keyVars())
    expect_identical(colnames(get.sdcMicroObj(obj$sdcObj, "manipNumVars")), num_vars)
    # no inputs per subset size of key variables, numerical results first
    expect_length(obj$rbs, 0)
    expect_length(obj$sls, 0)
    expect_identical(obj$cur_selection_results, "btn_results_8")
    # the generated script reproduces the object
    expect_match(obj$code_setup, "sdcObj <- createSdcObj(dat=inputdata,", fixed = TRUE)
    expect_match(obj$code_setup, "keyVars=NULL", fixed = TRUE)
  }))
})

test_that("the summary page leaves out the sections on categorical key variables", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    expect_gt(nchar(html(output$ui_sdcObj_summary)), 0)
    expect_match(html(output$show_info_general), "numerical key variables only")
    expect_identical(html(output$show_info_recodes), "")
    expect_identical(html(output$show_info_kanon), "")
    expect_identical(html(output$show_info_catrisk), "")
    expect_match(html(output$show_info_risk), "Risk measures for numerical key variables")
    expect_identical(html(output$risk_sb_anonymize), "")

    # unchanged with categorical key variables
    obj$sdcObj <- cat_sdc()
    session$flushReact()
    expect_true(has_keyVars())
    expect_match(html(output$show_info_kanon), "Information on k-anonymity")
    expect_match(html(output$show_info_catrisk), "Risk measures for categorical key variables")
    expect_match(html(output$risk_sb_anonymize), "k-anonymity")
    expect_false(grepl("numerical key variables only", html(output$show_info_general)))
  }))
})

test_that("pages that need categorical key variables show a notice", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    # linked variables, recoding, k-anonymity, suppression by risk
    for (i in c(3, 5, 6, 9)) {
      obj$cur_selection_anon <- paste0("btn_sel_anon_", i)
      session$flushReact()
      expect_match(html(output$ui_main_anon), notice, info = paste("page", i))
      expect_identical(html(output$ui_main_anon_header), "", info = paste("page", i))
    }
    # numerical methods and PRAM are offered as usual
    for (i in c(7, 8, 10:13)) {
      obj$cur_selection_anon <- paste0("btn_sel_anon_", i)
      session$flushReact()
      expect_false(grepl(notice, html(output$ui_main_anon)), info = paste("page", i))
      expect_gt(nchar(html(output$ui_main_anon)), 0)
    }
    for (i in 1:7) {
      obj$cur_selection_results <- paste0("btn_results_", i)
      session$flushReact()
      expect_match(html(output$ui_results_main), notice, info = paste("page", i))
    }
    for (i in 8:10) {
      obj$cur_selection_results <- paste0("btn_results_", i)
      session$flushReact()
      expect_false(grepl(notice, html(output$ui_results_main)), info = paste("page", i))
    }

    # with categorical key variables no page shows the notice
    obj$sdcObj <- cat_sdc()
    session$flushReact()
    for (i in 2:13) {
      obj$cur_selection_anon <- paste0("btn_sel_anon_", i)
      session$flushReact()
      expect_false(grepl(notice, html(output$ui_main_anon)), info = paste("page", i))
    }
    for (i in 1:10) {
      obj$cur_selection_results <- paste0("btn_results_", i)
      session$flushReact()
      expect_false(grepl(notice, html(output$ui_results_main)), info = paste("page", i))
    }
  }))
})

test_that("the pages for numerical variables render for a problem without categorical key variables", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- num_sdc()
    for (id in c("ui_microaggregation", "ui_noise", "ui_rankswap", "ui_topbotcoding_num",
                 "ui_resnum_comparison", "ui_resnum_numrisk", "ui_resnum_infoloss",
                 "ui_pram_simple", "ui_pram_expert", "ui_sdcObj_explorevars")) {
      res <- tryCatch(html(output[[id]]), error = function(e) paste("ERROR:", conditionMessage(e)))
      expect_false(grepl("^ERROR", res), info = paste(id, res))
    }
  }))
})
