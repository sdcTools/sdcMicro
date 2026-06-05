# Early stopping in the AI_applyAnonymization() refinement loop.
#
# The LLM call (query_llm) and the SDC primitives (execute_tool_calls,
# localSuppression, ai_utility_score, ...) are mocked so the loop's control flow
# is exercised deterministically -- no network, no API key. A real sdcMicroObj is
# built so the function receives a valid input; all per-strategy work is mocked to
# pass the object through, and ai_utility_score returns a scripted sequence of
# utility scores so we control exactly when the loop should stall.

make_sdc <- function() {
  data(testdata2, package = "sdcMicro")
  createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
}

# Closure returning a deterministic sequence of utility scores; the call counter
# `i` in its environment doubles as "number of strategies evaluated".
score_sequence <- function(totals) {
  i <- 0L
  function(sdcObj_orig, sdcObj_anon, weights = NULL) {
    i <<- i + 1L
    list(total = totals[i], suppression_rate = 0, category_loss = 0, il1 = 0)
  }
}

# Constant mocks shared across tests.
.m_noop_list   <- function(...) list()
.m_prompt      <- function(...) "prompt"
.m_query       <- function(...) "{}"
.m_strategies3 <- function(...) list(
  list(name = "a", reasoning = "", calls = list()),
  list(name = "b", reasoning = "", calls = list()),
  list(name = "c", reasoning = "", calls = list()))
.m_single      <- function(...) list(name = "r", reasoning = "", calls = list())
.m_passthru    <- function(sdcObj, calls) sdcObj
.m_locsupp     <- function(obj, k, ...) obj

test_that("refinement stops early after `patience` stalled iterations", {
  sdc <- make_sdc()
  # batch best = 0.10; no refinement ever beats it -> stop after 1 stalled round
  fake_score <- score_sequence(c(0.10, 0.20, 0.30, 0.15, 0.16, 0.17, 0.18))
  local_mocked_bindings(
    summarize_sdcObj_structure = .m_noop_list, get_tool_schemas = .m_noop_list,
    build_agentic_prompt = .m_prompt, build_refinement_prompt = .m_prompt,
    query_llm = .m_query, parse_strategies_json = .m_strategies3,
    parse_single_strategy_json = .m_single, execute_tool_calls = .m_passthru,
    localSuppression = .m_locsupp, ai_utility_score = fake_score,
    .package = "sdcMicro")

  expect_message(
    AI_applyAnonymization(sdc, provider = "custom", base_url = "http://localhost",
                          n_strategies = 3, max_iter = 5, patience = 1L, tol = 1e-3,
                          confirm = FALSE, verbose = TRUE, generateReport = FALSE),
    "Early stopping")
  # 3 batch evaluations + exactly 1 refinement before the early stop
  expect_equal(environment(fake_score)$i, 4L)
})

test_that("refinement continues while it keeps improving", {
  sdc <- make_sdc()
  # batch best = 0.30; each refinement strictly improves -> run all max_iter rounds
  fake_score <- score_sequence(c(0.30, 0.40, 0.50, 0.25, 0.20, 0.15))
  local_mocked_bindings(
    summarize_sdcObj_structure = .m_noop_list, get_tool_schemas = .m_noop_list,
    build_agentic_prompt = .m_prompt, build_refinement_prompt = .m_prompt,
    query_llm = .m_query, parse_strategies_json = .m_strategies3,
    parse_single_strategy_json = .m_single, execute_tool_calls = .m_passthru,
    localSuppression = .m_locsupp, ai_utility_score = fake_score,
    .package = "sdcMicro")

  AI_applyAnonymization(sdc, provider = "custom", base_url = "http://localhost",
                        n_strategies = 3, max_iter = 3, patience = 1L, tol = 1e-3,
                        confirm = FALSE, verbose = FALSE, generateReport = FALSE)
  # 3 batch + 3 refinements (no early stop) = 6
  expect_equal(environment(fake_score)$i, 6L)
})

test_that("patience > 1 tolerates a transient stall", {
  sdc <- make_sdc()
  # best=0.10; refine1 stalls, refine2 improves (reset), refine3+4 stall -> stop at 2 consecutive
  fake_score <- score_sequence(c(0.10, 0.20, 0.30, 0.20, 0.05, 0.06, 0.07))
  local_mocked_bindings(
    summarize_sdcObj_structure = .m_noop_list, get_tool_schemas = .m_noop_list,
    build_agentic_prompt = .m_prompt, build_refinement_prompt = .m_prompt,
    query_llm = .m_query, parse_strategies_json = .m_strategies3,
    parse_single_strategy_json = .m_single, execute_tool_calls = .m_passthru,
    localSuppression = .m_locsupp, ai_utility_score = fake_score,
    .package = "sdcMicro")

  expect_message(
    AI_applyAnonymization(sdc, provider = "custom", base_url = "http://localhost",
                          n_strategies = 3, max_iter = 10, patience = 2L, tol = 1e-3,
                          confirm = FALSE, verbose = TRUE, generateReport = FALSE),
    "Early stopping")
  # 3 batch + 4 refinements = 7
  expect_equal(environment(fake_score)$i, 7L)
})

test_that("tol and patience are validated", {
  sdc <- make_sdc()
  expect_error(
    AI_applyAnonymization(sdc, provider = "custom", base_url = "http://localhost",
                          patience = 0, confirm = FALSE, generateReport = FALSE))
  expect_error(
    AI_applyAnonymization(sdc, provider = "custom", base_url = "http://localhost",
                          tol = -1, confirm = FALSE, generateReport = FALSE))
})
