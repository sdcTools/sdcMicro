# sdcApp: the sidebar of the Anonymize tab. Its buttons are observed by observers
# that are created each time the sidebar is rendered. Clicking the entry that is
# already active and then another entry left two buttons with a non-zero value;
# the observers created by the next rendering then fired at once, each set its
# own entry, and the session never returned.
# The test does not provoke the endless loop itself: it checks that an observer does
# not fire when it is created, which is what the loop needs.
# The server is driven with shiny::testServer(); nothing is rendered in a browser.

skip_app <- function() {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not(utils::packageVersion("shiny") >= "1.5.0", "shiny::testServer needs shiny >= 1.5.0")
}
app_dir <- function() system.file("shiny", "sdcApp", package = "sdcMicro")
html <- function(x) paste(as.character(x), collapse = "")
quiet_sliders <- function(code) {
  withCallingHandlers(code, warning = function(w) {
    if (grepl("returning -Inf|less than or equal to `max`", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

test_that("the observers of the sidebar buttons do not fire when they are created", {
  skip_app()
  quiet_sliders(shiny::testServer(shiny::shinyAppDir(app_dir()), {
    data("Tarragona", package = "sdcMicro")
    obj$sdcObj <- createSdcObj(Tarragona[1:100, ], numVars = c("SALES", "NET.PROFIT"))
    # a button with a non-zero value before the sidebar and its observers exist
    session$setInputs(btn_sel_anon_3 = 1)
    html(output$ui_anonymize_sidebar_left)
    expect_match(html(output$ui_sel_anon_btns), "btn_sel_anon_11", fixed = TRUE)
    session$flushReact()
    ok <- identical(obj$cur_selection_anon, "btn_sel_anon_1")
    expect_true(ok)
    # only then is it safe to go on: with observers that fire on creation the
    # next step never returns (two buttons with a non-zero value)
    if (ok) {
      session$setInputs(btn_sel_anon_11 = 1)
      html(output$ui_sel_anon_btns)
      expect_identical(obj$cur_selection_anon, "btn_sel_anon_11")
      session$setInputs(btn_sel_anon_2 = 1)
      html(output$ui_sel_anon_btns)
      expect_identical(obj$cur_selection_anon, "btn_sel_anon_2")
    }
  }))
})
