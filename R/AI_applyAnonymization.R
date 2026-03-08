#' AI_applyAnonymization: Automatically apply anonymization strategy using LLM
#'
#' Uses an agentic loop to explore multiple anonymization strategies.
#' The LLM proposes strategies as structured tool calls, each is evaluated
#' with a combined utility score, and the best is selected.
#'
#' @param sdcObj An object of class sdcMicroObj.
#' @param k Desired k-anonymity level (default 3).
#' @param verbose If \code{TRUE}, prints progress and scores for each strategy.
#' @param model LLM model identifier. If \code{NULL}, a default is chosen per provider.
#' @param api_key API key. If \code{NULL}, auto-detected from environment variables.
#' @param provider LLM provider: \code{"openai"} (default), \code{"anthropic"}, or
#'   \code{"custom"} for any OpenAI-compatible endpoint.
#' @param base_url Base URL for the API endpoint. Required when \code{provider = "custom"}.
#' @param confirm Logical; if \code{TRUE} (default) and session is interactive, shows the
#'   best strategy and asks for confirmation before applying.
#' @param max_iter Number of refinement iterations after the initial batch (default 2).
#' @param n_strategies Number of strategies in the initial batch (default 3).
#' @param weights Numeric vector of length 3: weights for suppression rate,
#'   category loss, and IL1 in the utility score. Default \code{c(1/3, 1/3, 1/3)}.
#' @param generateReport If \code{TRUE}, generates internal and external reports.
#' @return Modified sdcMicroObj with the best anonymization strategy applied.
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' if (interactive() && nzchar(Sys.getenv("OPENAI_API_KEY"))) {
#'   library(sdcMicro)
#'   data(testdata)
#'   sdc <- AI_createSdcObj(dat = testdata, policy = "open", confirm = FALSE)
#'   sdc <- AI_applyAnonymization(sdcObj = sdc, k = 3, verbose = TRUE, confirm = FALSE)
#' }
#' }
AI_applyAnonymization <- function(sdcObj, k = 3, verbose = TRUE,
                                  model = NULL, api_key = NULL,
                                  provider = c("openai", "anthropic", "custom"),
                                  base_url = NULL, confirm = TRUE,
                                  max_iter = 2, n_strategies = 3,
                                  weights = c(1/3, 1/3, 1/3),
                                  generateReport = TRUE) {
  provider <- match.arg(provider)
  summary_info <- summarize_sdcObj_structure(sdcObj, k)
  tool_schemas <- get_tool_schemas()

  # Common LLM call args
  llm_args <- list(provider = provider, model = model, api_key = api_key, base_url = base_url)

  # ====== Phase 1: Batch ======
  if (verbose) message("=== Batch phase: requesting ", n_strategies, " strategies ===")

  prompt <- build_agentic_prompt(summary_info, k, n_strategies, tool_schemas)

  # Try native tool calling for openai/anthropic, text fallback for custom
  use_native_tools <- provider %in% c("openai", "anthropic")

  if (use_native_tools) {
    response <- query_llm(
      prompt = prompt,
      system_prompt = "You are an expert in statistical disclosure control.",
      provider = provider, model = model, api_key = api_key,
      base_url = base_url, tools = tool_schemas
    )
    # Native tool calling returns tool_calls directly -- but for batch we need
    # multiple strategies. The prompt asks for JSON with strategies array even
    # with native tools, so we may get text content with JSON.
    # Parse the content as JSON strategies if tool_calls is empty
    if (length(response$tool_calls) > 0) {
      # Single strategy from tool calls
      strategies <- list(list(
        name = "native",
        reasoning = if (!is.null(response$content)) response$content else "",
        calls = response$tool_calls
      ))
    } else if (!is.null(response$content)) {
      strategies <- tryCatch(
        parse_strategies_json(response$content),
        error = function(e) {
          stop("Failed to parse batch strategies from LLM: ", e$message, call. = FALSE)
        }
      )
    } else {
      stop("LLM returned neither tool calls nor content.", call. = FALSE)
    }
  } else {
    # Text/JSON fallback
    content <- query_llm(
      prompt = prompt,
      system_prompt = "You are an expert in statistical disclosure control.",
      provider = provider, model = model, api_key = api_key,
      base_url = base_url
    )
    strategies <- parse_strategies_json(content)
  }

  # Evaluate each strategy
  results <- list()
  for (i in seq_along(strategies)) {
    strat <- strategies[[i]]
    if (verbose) {
      name <- if (!is.null(strat$name)) strat$name else paste("Strategy", i)
      message(sprintf("  Evaluating %s...", name))
    }

    sdcObj_copy <- sdcObj
    sdcObj_copy <- tryCatch(
      execute_tool_calls(sdcObj_copy, strat$calls),
      error = function(e) {
        if (verbose) message(sprintf("    Failed: %s", e$message))
        NULL
      }
    )
    if (is.null(sdcObj_copy)) next

    sdcObj_copy <- localSuppression(sdcObj_copy, k = k)
    score <- ai_utility_score(sdcObj, sdcObj_copy, weights)

    results[[length(results) + 1]] <- list(
      strategy = strat, sdcObj = sdcObj_copy, score = score
    )

    if (verbose) {
      message(sprintf("    U=%.4f (S=%.4f, C=%.4f, IL1=%.4f)",
                      score$total, score$suppression_rate,
                      score$category_loss, score$il1))
    }
  }

  if (length(results) == 0) {
    stop("All strategies failed. Cannot proceed.", call. = FALSE)
  }

  # Find best
  best_idx <- which.min(vapply(results, function(r) r$score$total, numeric(1)))
  best <- results[[best_idx]]

  # ====== Phase 2: Refinement ======
  for (iter in seq_len(max_iter)) {
    if (verbose) message(sprintf("=== Refinement iteration %d/%d ===", iter, max_iter))

    ref_prompt <- build_refinement_prompt(results, summary_info, k)

    # Refinement always uses text/JSON mode (no native tools) for reliable parsing
    ref_content <- query_llm(
      prompt = ref_prompt,
      system_prompt = "You are an expert in statistical disclosure control. Always respond with valid JSON only, no markdown or prose.",
      provider = provider, model = model, api_key = api_key,
      base_url = base_url
    )
    # Handle query_llm returning a list (native mode) vs character (text mode)
    if (is.list(ref_content)) ref_content <- ref_content$content
    refined <- tryCatch(
      parse_single_strategy_json(ref_content),
      error = function(e) {
        if (verbose) message("  Could not parse refinement response, skipping.")
        NULL
      }
    )
    if (is.null(refined)) next

    sdcObj_copy <- sdcObj
    sdcObj_copy <- tryCatch(
      execute_tool_calls(sdcObj_copy, refined$calls),
      error = function(e) {
        if (verbose) message(sprintf("  Refinement failed: %s", e$message))
        NULL
      }
    )
    if (is.null(sdcObj_copy)) next

    sdcObj_copy <- localSuppression(sdcObj_copy, k = k)
    score <- ai_utility_score(sdcObj, sdcObj_copy, weights)

    if (verbose) {
      message(sprintf("  U=%.4f (S=%.4f, C=%.4f, IL1=%.4f)",
                      score$total, score$suppression_rate,
                      score$category_loss, score$il1))
    }

    result_entry <- list(strategy = refined, sdcObj = sdcObj_copy, score = score)
    results[[length(results) + 1]] <- result_entry

    if (score$total < best$score$total) {
      best <- result_entry
      if (verbose) message("  -> New best!")
    }
  }

  # ====== Show result + confirm ======
  if (verbose) {
    message(sprintf("\n=== Best strategy: '%s' (U=%.4f) ===",
                    if (!is.null(best$strategy$name)) best$strategy$name else "best",
                    best$score$total))
    if (!is.null(best$strategy$reasoning) && nzchar(best$strategy$reasoning)) {
      cat(strwrap(best$strategy$reasoning, width = 75), sep = "\n")
      cat("\n")
    }
    message(sprintf("  Suppression rate: %.4f", best$score$suppression_rate))
    message(sprintf("  Category loss:    %.4f", best$score$category_loss))
    message(sprintf("  IL1:              %.4f", best$score$il1))
  }

  if (confirm && interactive()) {
    answer <- readline("Apply this strategy? [Y/n/q] ")
    answer <- tolower(trimws(answer))
    if (answer %in% c("q", "quit")) {
      message("Aborted by user.")
      return(sdcObj)
    }
    if (answer %in% c("n", "no")) {
      message("Rejected. Returning original sdcObj unchanged.")
      return(sdcObj)
    }
  }

  sdcObj <- best$sdcObj

  if (generateReport) {
    report(sdcObj, filename = "anonymization_internal.html", internal = TRUE)
    report(sdcObj, filename = "anonymization_external.html", internal = FALSE)
  }

  return(sdcObj)
}
