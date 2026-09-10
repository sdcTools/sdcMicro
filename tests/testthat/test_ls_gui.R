## Phase D: what sdcApp needs from the package, and what the app generates.
##
## The GUI is the place where a silently under-protected file would do real
## damage, so the checks that matter here are (a) an engine result is never
## handed on unverified, (b) the audit trail says what the run achieved, and
## (c) the generated script reproduces the run.

toy_sdc <- function(alpha = 1) {
  data(testdata2, package = "sdcMicro")
  createSdcObj(testdata2, keyVars = c("urbrur", "roof", "walls"),
               w = "sampling_weight", alpha = alpha)
}

# ---- which backends are there? ---------------------------------------------

test_that("ls_solvers reports the installed MILP backends in preference order", {
  s <- ls_solvers()
  expect_type(s, "character")
  expect_true(all(s %in% c("highs", "scip", "gurobi")))
  expect_identical(s, c("highs", "scip", "gurobi")[c("highs", "scip", "gurobi") %in% s])
  for (b in c("highs", "scip", "gurobi")) {
    expect_identical(b %in% s, requireNamespace(b, quietly = TRUE), info = b)
  }
})

# ---- no engine result leaves the dispatcher unverified ----------------------

test_that("ls_verify_release accepts a protected file and refuses an unprotected one", {
  x <- data.frame(a = c("1", "1", "2", "2"), b = c("x", "x", "y", "y"))
  expect_true(sdcMicro:::ls_verify_release(x, keyVars = names(x), k = 2, method = "aggregate"))

  bad <- data.frame(a = c("1", "1", "2", "3"), b = c("x", "x", "y", "z"))
  expect_error(
    sdcMicro:::ls_verify_release(bad, keyVars = names(bad), k = 2, method = "aggregate"),
    "aggregate"
  )
  expect_error(
    sdcMicro:::ls_verify_release(bad, keyVars = names(bad), k = 2, method = "aggregate"),
    "2 record"
  )
})

test_that("the verification uses the alpha the file will be evaluated at", {
  ## record 2 is a partial contributor: worth alpha to the complete record 1,
  ## so record 1 reaches k = 2 at alpha = 1 and misses it at alpha = 0. (An
  ## all-missing contributor would be worth 0 to a complete receiver at any
  ## alpha, which is why the example blanks one cell, not both.)
  x <- data.frame(a = c("1", NA, "2", "2"), b = c("x", "x", "y", "y"))
  expect_true(sdcMicro:::ls_verify_release(x, keyVars = names(x), k = 2, alpha = 1,
                                method = "aggregate"))
  expect_error(sdcMicro:::ls_verify_release(x, keyVars = names(x), k = 2, alpha = 0,
                                 method = "aggregate"), "aggregate")
})

# ---- the audit line --------------------------------------------------------

test_that("ls_method_note stays silent for the original sweep", {
  expect_null(sdcMicro:::ls_method_note(list(method = "heuristic")))
  expect_null(sdcMicro:::ls_method_note(list()))
  expect_null(sdcMicro:::ls_method_note(NULL))
})

test_that("ls_method_note claims optimality only when it was proved", {
  n <- sdcMicro:::ls_method_note(list(method = "optimal", objective = 12, bound = 12,
                           gap = 0, status = "optimal", time = 3.42,
                           solver = "highs"))
  expect_match(n, "optimal")
  expect_match(n, "proved optimal")
  expect_match(n, "12")
  expect_match(n, "highs")
  expect_match(n, "3.4")

  g <- sdcMicro:::ls_method_note(list(method = "greedy2", objective = 20, bound = NA_real_,
                           gap = NA_real_, status = NA_character_,
                           time = NA_real_, solver = NA_character_))
  expect_match(g, "greedy2")
  expect_false(grepl("proved", g))
})

test_that("ls_method_note reports a truncated solve with its gap and bound", {
  n <- sdcMicro:::ls_method_note(list(method = "lns", objective = 40, bound = 35.2,
                           gap = 0.12, status = "time_limit", time = 60,
                           solver = "highs"))
  expect_match(n, "time limit")
  expect_match(n, "12")
  expect_match(n, "35.2")
  expect_false(grepl("proved", n))

  i <- sdcMicro:::ls_method_note(list(method = "optimal", objective = NA_real_,
                           bound = NA_real_, gap = NA_real_,
                           status = "infeasible", time = 0.2,
                           solver = "highs"))
  expect_match(i, "infeasible")
})

# ---- what the summary page and the report read -----------------------------

test_that("print(type = 'ls') carries the engine through to the GUI", {
  sdc <- kAnon(toy_sdc(), k = 2, method = "greedy2")
  res <- print(sdc, type = "ls", docat = FALSE)
  expect_identical(res$method, "greedy2")
  expect_match(res$note, "greedy2")
  ## the fields the audit trail shows
  expect_true(all(c("objective", "bound", "gap", "status", "time", "solver") %in%
                    names(res)))

  plain <- kAnon(toy_sdc(), k = 2)
  res2 <- print(plain, type = "ls", docat = FALSE)
  expect_identical(res2$method, "heuristic")
  expect_null(res2$note)
  ## nothing the app already read may have moved
  expect_true(all(c("supps", "suppsT", "threshold", "strataVars") %in% names(res2)))
})

test_that("the report records which engine produced the suppressions", {
  sdc <- kAnon(toy_sdc(), k = 2, method = "greedy2")
  rep <- sdcMicro:::calcReportData(sdc, internal = TRUE, title = "t",
                                   outdir = tempdir())
  ls <- sdcMicro:::get.reportObj(rep, "localSupps")
  expect_identical(ls$method, "greedy2")
  expect_match(ls$note, "greedy2")
})

# ---- the app: method choice, generated code, audit trail --------------------

skip_app <- function() {
  skip_if_not_installed("shiny")
  skip_if_not(utils::packageVersion("shiny") >= "1.5.0",
              "shiny::testServer needs shiny >= 1.5.0")
}

app_dir <- function() system.file("shiny", "sdcApp", package = "sdcMicro")

test_that("the app offers only engines that can run on this problem", {
  skip_app()
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- toy_sdc()
    m <- kAnon_methods()
    expect_identical(m$methods[1], "heuristic")
    expect_true("greedy2" %in% m$methods)
    if (length(ls_solvers()) > 0) {
      expect_true(all(c("optimal", "aggregate") %in% m$methods))
    } else {
      expect_false(any(c("optimal", "aggregate") %in% m$methods))
      expect_match(m$hint, "highs")
    }
    expect_identical(length(m$labels), length(m$methods))

    ## alpha != 1: only the aggregated model counts the way freqCalc does
    obj$sdcObj <- toy_sdc(alpha = 0.5)
    m2 <- kAnon_methods()
    expect_false("greedy2" %in% m2$methods)
    expect_false("optimal" %in% m2$methods)
    expect_match(m2$hint, "alpha")
  })
})

test_that("the app states how many records still violate the chosen k", {
  skip_app()
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    sdc <- toy_sdc()
    obj$sdcObj <- sdc
    session$setInputs(rb_kanon_useCombs = "No", sl_kanon_k = 3)
    fk <- get.sdcMicroObj(sdc, "risk")$individual[, "fk"]
    expect_identical(kAnon_violators(), sum(fk < 3))
  })
})

test_that("the k-anonymity panel renders the selector and its controls", {
  skip_app()
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- toy_sdc()
    session$setInputs(rb_show_importance = "No", rb_kanon_useCombs = "No",
                      sl_kanon_k = 3, rb_kanon_method = "heuristic")
    ## the nested outputs only exist once the panel itself has rendered
    expect_gt(nchar(paste(as.character(output$ui_kAnon), collapse = "")), 0)

    m <- paste(as.character(output$kanon_method), collapse = "")
    expect_match(m, 'id="rb_kanon_method"')
    expect_match(m, 'value="heuristic"')
    expect_match(m, "record\\(s\\) currently violate")
    ## solver controls belong to the exact engines only
    expect_false(grepl("sel_kanon_solver",
                       paste(as.character(output$kanon_method_opts), collapse = "")))

    skip_if_not(length(ls_solvers()) > 0, "no MILP backend installed")
    session$setInputs(rb_kanon_method = "optimal")
    o <- paste(as.character(output$kanon_method_opts), collapse = "")
    expect_match(o, "sel_kanon_solver")
    expect_match(o, "sl_kanon_timelimit")
    expect_match(o, "sl_kanon_maxper")
  })
})

test_that("the generated command is unchanged for the default engine", {
  skip_app()
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- toy_sdc()
    session$setInputs(kanon_strataV = "no stratification", rb_show_importance = "No",
                      rb_kanon_useCombs = "No", sl_kanon_k = 3,
                      rb_kanon_method = "heuristic")
    r <- code_kAnon()
    expect_false(grepl("method", r$cmd))
    expect_match(r$cmd, "importance=c\\(")
  })
})

test_that("a non-default engine reaches the generated command and runs", {
  skip_app()
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- toy_sdc()
    session$setInputs(kanon_strataV = "no stratification", rb_show_importance = "No",
                      rb_kanon_useCombs = "No", sl_kanon_k = 3,
                      rb_kanon_method = "greedy2")
    r <- code_kAnon()
    expect_match(r$cmd, 'method="greedy2"')
    ## the sweep's materialised default order is a tie-break, not a cost:
    ## it must not be handed to an engine that reads importance as a cost
    expect_false(grepl("importance=", r$cmd))
    expect_match(r$txt_action, "equally costly")

    sdcObj <- toy_sdc()
    eval(parse(text = r$cmd))
    expect_identical(print(sdcObj, type = "ls", docat = FALSE)$method, "greedy2")
  })
})

test_that("the button runs the engine and the audit trail records what it achieved", {
  skip_app()
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- toy_sdc()
    session$setInputs(kanon_strataV = "no stratification", rb_show_importance = "No",
                      rb_kanon_useCombs = "No", sl_kanon_k = 3,
                      rb_kanon_method = "greedy2")
    before <- sum(is.na(get.sdcMicroObj(obj$sdcObj, "manipKeyVars")))
    session$setInputs(btn_kanon = 1)

    expect_null(obj$last_error)
    expect_gt(sum(is.na(get.sdcMicroObj(obj$sdcObj, "manipKeyVars"))), before)
    ## the trail says which engine ran, not only that suppression was asked for
    expect_match(obj$lastaction, "greedy2")
    expect_identical(utils::tail(obj$anon_performed, 1), obj$lastaction)
    ## and the script tab reproduces the run
    expect_true(any(grepl('method="greedy2"', obj$code_anonymize, fixed = TRUE)))
  })
})

test_that("an importance vector the user set is still passed to the engine", {
  skip_app()
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- toy_sdc()
    session$setInputs(kanon_strataV = "no stratification", rb_show_importance = "Yes",
                      sel_importance_1 = "3", sel_importance_2 = "1",
                      sel_importance_3 = "2",
                      rb_kanon_useCombs = "No", sl_kanon_k = 3,
                      rb_kanon_method = "greedy2")
    r <- code_kAnon()
    expect_match(r$cmd, "importance=c\\(3,1,2\\)")
    expect_match(r$cmd, 'method="greedy2"')
  })
})

test_that("the exact engine carries its controls, and a warm start, into the command", {
  skip_app()
  skip_if_not(length(ls_solvers()) > 0, "no MILP backend installed")
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- toy_sdc()
    session$setInputs(kanon_strataV = "no stratification", rb_show_importance = "No",
                      rb_kanon_useCombs = "No", sl_kanon_k = 3,
                      rb_kanon_method = "optimal",
                      sel_kanon_solver = ls_solvers()[1],
                      sl_kanon_timelimit = 30, sl_kanon_maxper = 1)
    r <- code_kAnon()
    expect_match(r$cmd, 'method="optimal"')
    expect_match(r$cmd, "control=list\\(")
    expect_match(r$cmd, paste0('solver="', ls_solvers()[1], '"'))
    expect_match(r$cmd, "time_limit=30")
    expect_match(r$cmd, "max_per_record=1")
    ## without a warm start a truncated solve has no feasible fallback
    expect_match(r$cmd, 'warm_start="greedy2"')

    sdcObj <- toy_sdc()
    eval(parse(text = r$cmd))
    ls <- print(sdcObj, type = "ls", docat = FALSE)
    expect_identical(ls$method, "optimal")
    expect_match(ls$note, "solver")
  })
})

test_that("lifting the cap is what the user must do, not the default", {
  skip_app()
  skip_if_not(length(ls_solvers()) > 0, "no MILP backend installed")
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    obj$sdcObj <- toy_sdc()
    session$setInputs(kanon_strataV = "no stratification", rb_show_importance = "No",
                      rb_kanon_useCombs = "No", sl_kanon_k = 3,
                      rb_kanon_method = "optimal",
                      sel_kanon_solver = ls_solvers()[1],
                      sl_kanon_timelimit = 30, sl_kanon_maxper = 0)
    r <- code_kAnon()
    ## 0 on the slider is the explicit "no cap" step
    expect_false(grepl("max_per_record", r$cmd))
  })
})
