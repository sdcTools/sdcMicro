#' Create an sdcMicro Object with LLM Assistance
#'
#' This function uses a Large Language Model (LLM) to automatically classify variables
#' in a dataset into quasi-identifiers, sensitive variables, numerical variables, and more,
#' and passes the result to \code{createSdcObj()}. It optionally uses a codebook and policy context.
#'
#' @param dat A data.frame containing the microdata.
#' @param codebook Optional path to a codebook file (currently not parsed; placeholder for future use).
#' @param policy Data sharing policy context: \code{"open"} (default), \code{"restricted"}, or \code{"confidential"}.
#' @param model The LLM model to use. If \code{NULL}, a default is chosen per provider.
#' @param api_key API key. If \code{NULL}, auto-detected from environment variables.
#' @param provider LLM provider: \code{"openai"} (default), \code{"anthropic"}, or
#'   \code{"custom"} for any OpenAI-compatible endpoint (Ollama, Azure, vLLM, Groq, etc.).
#' @param base_url Base URL for the API endpoint. Required when \code{provider = "custom"}.
#' @param confirm Logical; if \code{TRUE} (default) and session is interactive, shows the
#'   proposed classification and asks for confirmation before creating the sdcMicroObj.
#' @param info Logical; if \code{TRUE}, prints the LLM classification result and reasoning.
#' @param ... Additional arguments passed to \code{createSdcObj()}.
#'
#' @return An object of class \code{sdcMicroObj}.
#' @author Matthias Templ
#' @export
#'
#' @examples
#' \dontrun{
#' data(testdata)
#' sdc <- AI_createSdcObj(dat = testdata, policy = "open")
#' sdc
#' }
AI_createSdcObj <- function(dat,
                            codebook = NULL,
                            policy = c("open", "restricted", "confidential"),
                            model = NULL,
                            api_key = NULL,
                            provider = c("openai", "anthropic", "custom"),
                            base_url = NULL,
                            confirm = TRUE,
                            info = TRUE,
                            ...) {
  policy <- match.arg(policy)
  provider <- match.arg(provider)
  meta <- extract_variable_metadata(dat, codebook)
  prompt <- build_llm_prompt(meta, policy, codebook)
  roles <- query_llm_for_roles(prompt, model = model, api_key = api_key,
                               provider = provider, base_url = base_url)
  
  # Validate and coerce variable types
  coerced <- character(0)
  if (!is.null(roles$pramVars)) {
    for (var in roles$pramVars) {
      if (!is.factor(dat[[var]]) && !is.character(dat[[var]])) {
        dat[[var]] <- as.factor(dat[[var]])
        coerced <- c(coerced, var)
      }
    }
  }
  if (!is.null(roles$numVars)) {
    for (var in roles$numVars) {
      if (!is.numeric(dat[[var]])) {
        dat[[var]] <- as.numeric(dat[[var]])
        coerced <- c(coerced, var)
      }
    }
  }
  if (!is.null(roles$keyVars)) {
    for (var in roles$keyVars) {
      if (!is.factor(dat[[var]])) {
        dat[[var]] <- as.factor(dat[[var]])
        coerced <- c(coerced, var)
      }
    }
  }
  if (length(coerced) > 0) {
    message("Coerced to appropriate type: ", paste(coerced, collapse = ", "))
  }
  if (inherits(roles$pramVars, "character") && length(roles$pramVars) == 0) {
    roles$pramVars <- NULL
  }
  if (inherits(roles$keyVars, "character") && length(roles$keyVars) == 0) {
    roles$keyVars <- NULL
  }
  if (inherits(roles$numVars, "character") && length(roles$numVars) == 0) {
    roles$numVars <- NULL
  }
  if (info) {
    if (!is.null(roles$reasoning)) {
      message("LLM reasoning:\n")
      cat(strwrap(roles$reasoning, width = 75), sep = "\n")
      cat("\n")
    }
    message("LLM classification result:\n")
    classification <- roles[setdiff(names(roles), "reasoning")]
    print(classification)
  }

  # Interactive confirmation
  if (confirm && interactive()) {
    answer <- readline("Accept this classification? [Y/n/q] ")
    answer <- tolower(trimws(answer))
    if (answer %in% c("q", "quit")) {
      message("Aborted by user.")
      return(invisible(NULL))
    }
    if (answer %in% c("n", "no")) {
      message("Classification rejected. Returning roles list for manual editing.")
      return(roles)
    }
  }

  sdc <- createSdcObj(dat = dat,
                      keyVars = roles$keyVars,
                      pramVars = roles$pramVars,
                      numVars = roles$numVars,
                      weightVar = roles$weightVar,
                      strataVar = roles$strataVar,
                      hhId = roles$hhId,
                      ...)
  return(sdc)
}