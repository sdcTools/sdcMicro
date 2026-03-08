#' Clean LLM-generated R code
#'
#' Replaces literal '\\n' with real line breaks, trims whitespace, and optionally indents.
#'
#' @param code_string Character string returned by LLM (with '\\n' line breaks).
#' @param indent Logical, whether to indent lines (for printing only).
#' @return Cleaned R code as a single string.
#' @keywords internal
clean_llm_code <- function(code_string, indent = FALSE) {
  lines <- unlist(strsplit(gsub("\\\\n", "\n", code_string), "\n"))
  lines <- trimws(lines)
  if (indent) {
    lines <- paste0("  ", lines)
  }
  paste(lines, collapse = "\n")
}

#' Summarize structure and k-anonymity violations from an sdcMicroObj
#' @param sdcObj An sdcMicroObj
#' @param k k-anonymity level
#' @return A list with keyVars, pramVars, numVars, k_violations, and keyVarLevels
#' @noRd
summarize_sdcObj_structure <- function(sdcObj, k = 3) {
  dat_names <- colnames(get.sdcMicroObj(sdcObj, type = "origData"))

  keyVars  <- dat_names[get.sdcMicroObj(sdcObj, type = "keyVars")]
  pramVars <- dat_names[get.sdcMicroObj(sdcObj, type = "pramVars")]
  numVars  <- dat_names[get.sdcMicroObj(sdcObj, type = "numVars")]

  # Compute k-anonymity violations from frequency counts
  fk <- sdcObj@risk$individual[, "fk"]
  k_violations <- sum(fk < k)

  # Collect exact factor levels for all key variables
  keyVarLevels <- lapply(keyVars, function(v) {
    var_data <- factor(get.sdcMicroObj(sdcObj, type = "origData")[[v]])
    levels(var_data)
  })
  names(keyVarLevels) <- keyVars

  list(
    keyVars      = keyVars,
    pramVars     = pramVars,
    numVars      = numVars,
    k_violations = k_violations,
    keyVarLevels = keyVarLevels
  )
}

#' Build anonymization strategy prompt from summary info
#'
#' Constructs a prompt to instruct an LLM to propose a valid anonymization plan
#' using sdcMicro functions with correct syntax and arguments.
#'
#' @param summary_info A list from \code{summarize_sdcObj_structure()}.
#' @param k Desired k-anonymity level.
#' @return A character string prompt.
#' @keywords internal
build_anonymization_prompt <- function(summary_info, k) {
  # Format key variable levels block
  levels_block <- paste(
    sprintf("- %s: c(%s)",
            names(summary_info$keyVarLevels),
            vapply(summary_info$keyVarLevels,
                   function(x) paste(sprintf('"%s"', x), collapse = ", "),
                   character(1))),
    collapse = "\n"
  )

  pram_info <- if (length(summary_info$pramVars) > 0)
    paste(summary_info$pramVars, collapse = ", ")
  else
    "(none defined - do not use PRAM)"

  paste(
    "You are an expert in data anonymization using the R package sdcMicro.",
    "",
    "The sdcMicro object has:",
    sprintf("- Key variables (categorical quasi-identifiers): %s",
            paste(summary_info$keyVars, collapse = ", ")),
    sprintf("- PRAM variables: %s", pram_info),
    sprintf("- Numerical variables: %s",
            paste(summary_info$numVars, collapse = ", ")),
    sprintf("- k-anonymity violations (k = %d): %d", k, summary_info$k_violations),
    "",
    "Exact levels of all key variables (use these in groupAndRename calls):",
    levels_block,
    "",
    "Create valid, executable R code for an anonymization plan using only sdcMicro functions.",
    "",
    "FUNCTION RULES:",
    "",
    "groupAndRename(sdcObj, var, before, after):",
    "- For recoding categorical key variables (factors) only.",
    "- `before` MUST contain ALL existing levels of the variable. Omitting any level causes an error.",
    "- `after` must have the same length as `before`.",
    "- Unchanged levels must be listed with the same value in `after`.",
    "- Apply selectively to reduce cardinality: focus on categories with low frequencies.",
    "- Do NOT apply to numeric/integer variables.",
    "",
    "globalRecode(sdcObj, column, breaks, labels):",
    "- For binning numeric variables ONLY (e.g., income, savings).",
    "- Do NOT use on factor/character variables or variables in keyVars (causes error).",
    "- Do NOT use on variables in numVars that are continuous (use microaggregation instead).",
    "",
    "localSuppression(sdcObj, k):",
    "- Enforces k-anonymity on all key variables automatically. No keyVars argument needed.",
    sprintf("- ALWAYS include this with k = %d. This is mandatory.", k),
    "",
    "microaggregation(sdcObj, variables, method):",
    "- For numerical variables. The argument is called `variables` (not `numVars`).",
    "- Use method = \"mdav\" for datasets under 20000 rows, \"onedims\" for larger.",
    "- Example: sdcObj <- microaggregation(sdcObj, variables = c(\"income\"), method = \"mdav\")",
    "",
    "addNoise(sdcObj, variables, noise):",
    "- Alternative to microaggregation for numerical variables.",
    "",
    if (length(summary_info$pramVars) > 0)
      "pram(sdcObj): Apply to PRAM variables."
    else
      "Do NOT apply pram() (no PRAM variables defined).",
    "",
    "RULES:",
    "1. Only use variable names listed above - no made-up names.",
    "2. Do not use column numbers (e.g. column = 1).",
    "3. Do not include markdown formatting, backticks, or comments in code.",
    sprintf("4. ALWAYS include localSuppression(sdcObj, k = %d) in the code.", k),
    "5. In groupAndRename(), ALL levels must appear in `before`.",
    "",
    "OUTPUT FORMAT:",
    "Return a valid JSON object with three fields:",
    "- \"reasoning\": explain why each anonymization method was chosen for each variable.",
    "- \"explanation\": plain-text summary (1-3 sentences).",
    "- \"code\": executable R code as a single string, use \"\\n\" for line breaks.",
    "",
    "Example:",
    "{",
    sprintf(paste0(
      "  \"reasoning\": \"Roof has 3 levels with low frequencies in level 2, so grouping reduces risk. ",
      "Income is continuous, so microaggregation preserves distribution while reducing precision.\",\n",
      "  \"explanation\": \"Recoded roof, applied local suppression and microaggregation.\",\n",
      "  \"code\": \"sdcObj <- groupAndRename(sdcObj, var = \\\"roof\\\", ",
      "before = c(\\\"1\\\",\\\"2\\\",\\\"3\\\"), after = c(\\\"1\\\",\\\"1\\\",\\\"3\\\"))\\n",
      "sdcObj <- localSuppression(sdcObj, k = %d)\\n",
      "sdcObj <- microaggregation(sdcObj, variables = c(\\\"income\\\"), method = \\\"mdav\\\")\""), k),
    "}",
    sep = "\n"
  )
}

#' Query LLM to suggest anonymization code
#' @param prompt Character string prompt.
#' @param model Model identifier (passed to \code{query_llm}).
#' @param api_key API key (passed to \code{query_llm}).
#' @param provider LLM provider (passed to \code{query_llm}).
#' @param base_url Base URL (passed to \code{query_llm}).
#' @noRd
query_llm_anonymization_plan <- function(prompt, model = NULL, api_key = NULL,
                                         provider = "openai", base_url = NULL) {
  full_content <- query_llm(
    prompt = prompt,
    system_prompt = "You are an anonymization expert.",
    provider = provider,
    model = model,
    api_key = api_key,
    base_url = base_url
  )

  # Strip markdown code fences
  full_content <- gsub("^```json\\s*|\\s*```$", "", full_content)

  parsed <- tryCatch(
    jsonlite::fromJSON(full_content),
    error = function(e) {
      fallback_code <- regmatches(full_content,
                                  regexpr("(?s)```R(.*?)```", full_content, perl = TRUE))
      fallback_code <- gsub("^```R\\s*|\\s*```$", "", fallback_code)
      if (length(fallback_code) > 0) {
        warning("LLM returned raw code block. Falling back to manual parsing.")
        return(list(explanation = "Fallback: explanation missing", code = fallback_code))
      }
      stop("Failed to parse LLM response: ", e$message,
           "\nRaw response:\n", full_content)
    }
  )
  parsed
}

#' Validate and patch incorrect globalRecode calls on factor keyVars
#' @noRd
patch_globalRecode_on_factors <- function(sdcObj, code) {
  code_lines <- unlist(strsplit(code, "\n"))
  orig_data <- get.sdcMicroObj(sdcObj, type = "origData")
  key_idx <- get.sdcMicroObj(sdcObj, type = "keyVars")
  var_types <- vapply(orig_data[key_idx], function(x) class(x)[1], character(1))

  factor_vars <- names(var_types[var_types %in% c("factor", "character")])

  new_lines <- vapply(code_lines, function(line) {
    for (var in factor_vars) {
      pattern <- paste0('globalRecode\\(.*column *= *"?', var, '"?')
      if (grepl(pattern, line)) {
        message(sprintf("Replacing invalid globalRecode() on factor variable '%s' with comment", var))
        return(paste0("# Removed invalid globalRecode on factor: ", var))
      }
    }
    line
  }, character(1), USE.NAMES = FALSE)

  paste(new_lines, collapse = "\n")
}

#' Remove globalRecode calls on numerical variables
#' @param code Character string of R code
#' @param numVarNames Character vector of numerical variable names
#' @noRd
remove_globalRecode_on_numVars <- function(code, numVarNames) {
  for (var in numVarNames) {
    code <- gsub(
      pattern = paste0('sdcObj <- globalRecode\\(sdcObj, column = "', var, '"[^\n]*\n?'),
      replacement = "",
      x = code
    )
  }
  code
}

#' Patch groupAndRename calls to include all factor levels
#' @noRd
patch_groupAndRename_missing_levels <- function(code_lines, sdcObj) {
  vapply(code_lines, function(line) {
    if (!grepl("groupAndRename\\(", line)) return(line)

    # Extract variable name
    var_match <- regmatches(line, regexpr('var *= *"[^"]+"', line))
    if (length(var_match) == 0) return(line)
    var_name <- gsub('var *= *"|"', "", var_match)

    # Get all existing levels
    var_levels <- levels(sdcObj@manipKeyVars[[var_name]])
    if (is.null(var_levels)) return(line)

    # Extract 'before' list
    before_match <- regmatches(line, regexpr('before *= *c\\([^)]+\\)', line))
    if (length(before_match) == 0) return(line)
    before_values <- gsub('before *= *c\\(|\\)|"', "", before_match)
    before_values <- trimws(unlist(strsplit(before_values, ",")))

    # Add any missing levels with identity mapping
    missing_levels <- setdiff(var_levels, before_values)
    if (length(missing_levels) == 0) return(line)

    message(sprintf("Patching groupAndRename() for '%s': adding %d missing levels",
                    var_name, length(missing_levels)))

    after_match <- regmatches(line, regexpr('after *= *c\\([^)]+\\)', line))
    after_values <- gsub('after *= *c\\(|\\)|"', "", after_match)
    after_values <- trimws(unlist(strsplit(after_values, ",")))

    before_all <- c(before_values, missing_levels)
    after_all  <- c(after_values, missing_levels)

    sprintf(
      'sdcObj <- groupAndRename(sdcObj, var = "%s", before = c(%s), after = c(%s))',
      var_name,
      paste(sprintf('"%s"', before_all), collapse = ", "),
      paste(sprintf('"%s"', after_all), collapse = ", ")
    )
  }, character(1), USE.NAMES = FALSE)
}

#' Validate that groupAndRename calls include all factor levels
#' @noRd
check_groupAndRename_validity <- function(sdcObj, code_string) {
  keyVars <- get.sdcMicroObj(sdcObj, type = "keyVars")
  origData <- get.sdcMicroObj(sdcObj, type = "origData")
  var_names <- colnames(origData)[keyVars]

  matches <- gregexpr('groupAndRename\\(sdcObj, var = \\"(.*?)\\".*?before = c\\((.*?)\\)',
                       code_string)
  extracted <- regmatches(code_string, matches)[[1]]

  for (expr in extracted) {
    var <- sub('.*var = \\"(.*?)\\".*', '\\1', expr)
    before_raw <- sub('.*before = c\\((.*?)\\).*', '\\1', expr)
    before_vals <- gsub('"', '', strsplit(before_raw, ',\\s*')[[1]])

    if (!var %in% var_names) {
      warning(sprintf("Variable '%s' not found in original keyVars.", var))
      next
    }

    factor_levels <- levels(origData[[var]])
    missing <- setdiff(factor_levels, before_vals)
    if (length(missing) > 0) {
      stop(sprintf(
        "groupAndRename() for '%s' is missing level(s): %s",
        var, paste(missing, collapse = ", ")
      ))
    }
  }
}

#' Parse multiple strategies from LLM JSON response
#' @param content Character string with JSON containing a "strategies" array.
#' @return List of strategies, each with name, reasoning, calls.
#' @noRd
parse_strategies_json <- function(content) {
  content <- gsub("^```json\\s*|\\s*```$", "", trimws(content))
  parsed <- tryCatch(
    jsonlite::fromJSON(content, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse strategies JSON: ", e$message,
           "\nResponse:\n", substr(content, 1, 500), call. = FALSE)
    }
  )

  # Handle both {"strategies": [...]} and bare [...]
  strats <- if (!is.null(parsed$strategies)) parsed$strategies else parsed

  if (!is.list(strats) || length(strats) == 0) {
    stop("No strategies found in LLM response.", call. = FALSE)
  }

  lapply(strats, function(s) {
    list(
      name = if (!is.null(s$name)) s$name else "unnamed",
      reasoning = if (!is.null(s$reasoning)) s$reasoning else "",
      calls = lapply(s$calls, function(c) {
        # Normalize: ensure 'tool' field exists
        if (is.null(c$tool)) stop("Tool call missing 'tool' field.", call. = FALSE)
        c
      })
    )
  })
}

#' Parse a single strategy from LLM JSON response
#' @param content Character string with JSON containing a single strategy.
#' @return A strategy list with name, reasoning, calls.
#' @noRd
parse_single_strategy_json <- function(content) {
  content <- gsub("^```json\\s*|\\s*```$", "", trimws(content))
  parsed <- tryCatch(
    jsonlite::fromJSON(content, simplifyVector = FALSE),
    error = function(e) {
      stop("Failed to parse strategy JSON: ", e$message,
           "\nResponse:\n", substr(content, 1, 500), call. = FALSE)
    }
  )

  list(
    name = if (!is.null(parsed$name)) parsed$name else "refined",
    reasoning = if (!is.null(parsed$reasoning)) parsed$reasoning else "",
    calls = lapply(parsed$calls, function(c) {
      if (is.null(c$tool)) stop("Tool call missing 'tool' field.", call. = FALSE)
      c
    })
  )
}

#' Build prompt for agentic batch strategy generation
#'
#' Constructs a prompt instructing the LLM to propose multiple anonymization
#' strategies using structured tool calls. Each strategy is a sequence of
#' sdcMicro operations (groupAndRename, microaggregation, etc.).
#' localSuppression is applied automatically and should NOT be included.
#'
#' @param summary_info List from \code{summarize_sdcObj_structure()}.
#' @param k Desired k-anonymity level.
#' @param n_strategies Number of strategies to propose (default 3).
#' @param tool_schemas List from \code{get_tool_schemas()} (used for text fallback).
#' @return Character string prompt.
#' @keywords internal
build_agentic_prompt <- function(summary_info, k, n_strategies = 3, tool_schemas = NULL) {
  # Format key variable levels block (same as build_anonymization_prompt)
  levels_block <- paste(
    sprintf("- %s (%d levels): c(%s)",
            names(summary_info$keyVarLevels),
            vapply(summary_info$keyVarLevels, length, integer(1)),
            vapply(summary_info$keyVarLevels,
                   function(x) paste(sprintf('"%s"', x), collapse = ", "),
                   character(1))),
    collapse = "\n"
  )

  pram_info <- if (length(summary_info$pramVars) > 0)
    paste(summary_info$pramVars, collapse = ", ")
  else
    "(none)"

  # Build the prompt
  paste(
    "You are an expert in statistical disclosure control using the R package sdcMicro.",
    "",
    "The sdcMicro object has:",
    sprintf("- Key variables (categorical quasi-identifiers): %s",
            paste(summary_info$keyVars, collapse = ", ")),
    sprintf("- PRAM variables: %s", pram_info),
    sprintf("- Numerical variables: %s",
            if (length(summary_info$numVars) > 0) paste(summary_info$numVars, collapse = ", ") else "(none)"),
    sprintf("- Current k-anonymity violations (k=%d): %d records", k, summary_info$k_violations),
    "",
    "Exact levels of all key variables:",
    levels_block,
    "",
    sprintf("Propose %d DIFFERENT anonymization strategies, ranging from conservative (minimal recoding) to aggressive (heavy recoding).", n_strategies),
    "",
    "Each strategy is a sequence of tool calls. Available tools:",
    "- groupAndRename(var, before, after): Merge factor levels of a key variable. 'before' must list ALL levels.",
    "- microaggregation(variables, method): For numerical variables. method = 'mdav' or 'onedims'.",
    "- addNoise(variables, noise): Add noise to numerical variables.",
    if (length(summary_info$pramVars) > 0) "- pram(): Apply post-randomization to PRAM variables." else "",
    "- topBotCoding(column, value, replacement, kind): Top/bottom coding for numerical variables.",
    "",
    "IMPORTANT RULES:",
    sprintf("- Do NOT include localSuppression. It is applied automatically after each strategy with k=%d.", k),
    "- In groupAndRename, 'before' must contain ALL existing levels of the variable.",
    "- Only use variable names listed above.",
    "- Conservative strategy: recode only high-cardinality variables with many rare levels.",
    "- Aggressive strategy: recode most key variables to reduce cardinality substantially.",
    "",
    sprintf("Return a JSON object with a 'strategies' array containing exactly %d strategies.", n_strategies),
    "Each strategy has 'name' (string), 'reasoning' (string), and 'calls' (array of tool calls).",
    "Each tool call has 'tool' (string) and the tool's parameters.",
    "",
    "Example:",
    "{",
    "  \"strategies\": [",
    "    {",
    "      \"name\": \"conservative\",",
    "      \"reasoning\": \"Only recode roof which has rare levels.\",",
    "      \"calls\": [",
    "        {\"tool\": \"groupAndRename\", \"var\": \"roof\", \"before\": [\"2\",\"4\",\"5\",\"6\",\"9\"], \"after\": [\"2\",\"4\",\"5\",\"5\",\"9\"]}",
    "      ]",
    "    }",
    "  ]",
    "}",
    sep = "\n"
  )
}

#' Build prompt for refinement iteration
#'
#' Shows the LLM the utility scores from previous strategies and asks
#' it to propose an improved strategy that minimizes the total score.
#'
#' @param results List of previous results, each with \code{$strategy} and \code{$score}.
#' @param summary_info List from \code{summarize_sdcObj_structure()}.
#' @param k Desired k-anonymity level.
#' @return Character string prompt.
#' @keywords internal
build_refinement_prompt <- function(results, summary_info, k) {
  # Format previous results
  results_block <- vapply(seq_along(results), function(i) {
    r <- results[[i]]
    s <- r$score
    name <- if (!is.null(r$strategy$name)) r$strategy$name else paste("Strategy", i)
    sprintf(
      "  %s: U=%.4f (suppression=%.4f, category_loss=%.4f, IL1=%.4f)\n    Calls: %s",
      name, s$total, s$suppression_rate, s$category_loss, s$il1,
      paste(vapply(r$strategy$calls, function(c) c$tool, character(1)), collapse = " -> ")
    )
  }, character(1))

  # Find best and worst
  scores <- vapply(results, function(r) r$score$total, numeric(1))
  best_idx <- which.min(scores)
  best <- results[[best_idx]]

  # Levels block
  levels_block <- paste(
    sprintf("- %s: %d levels",
            names(summary_info$keyVarLevels),
            vapply(summary_info$keyVarLevels, length, integer(1))),
    collapse = "\n"
  )

  # Full levels block for refinement
  full_levels_block <- paste(
    sprintf("- %s: c(%s)",
            names(summary_info$keyVarLevels),
            vapply(summary_info$keyVarLevels,
                   function(x) paste(sprintf('"%s"', x), collapse = ", "),
                   character(1))),
    collapse = "\n"
  )

  paste(
    "You are refining an anonymization strategy for an sdcMicro object.",
    "",
    sprintf("Target: k=%d anonymity (localSuppression applied automatically).", k),
    "",
    "Key variables with their exact levels:",
    full_levels_block,
    "",
    "Previous strategies and their utility scores (lower is better):",
    paste(results_block, collapse = "\n"),
    "",
    sprintf("The best so far is '%s' with U=%.4f.",
            if (!is.null(best$strategy$name)) best$strategy$name else paste("Strategy", best_idx),
            best$score$total),
    "",
    "Analyze the scores:",
    "- High suppression_rate means too many NAs from localSuppression. Fix: more groupAndRename to reduce unique combinations.",
    "- High category_loss means too aggressive recoding. Fix: merge fewer levels.",
    "- High IL1 means numerical variables were over-perturbed.",
    "",
    "Propose ONE improved strategy that reduces the total score U.",
    "Return ONLY a valid JSON object (no markdown, no prose) with 'name', 'reasoning', and 'calls'.",
    "Each call in 'calls' must be an object with 'tool' and tool parameters.",
    "Do NOT include localSuppression in calls.",
    "",
    "Example:",
    "{",
    "  \"name\": \"balanced\",",
    "  \"reasoning\": \"Merge rare roof levels to reduce suppression.\",",
    "  \"calls\": [",
    "    {\"tool\": \"groupAndRename\", \"var\": \"roof\", \"before\": [\"2\",\"4\",\"5\",\"6\",\"9\"], \"after\": [\"2\",\"4\",\"5\",\"5\",\"9\"]}",
    "  ]",
    "}",
    sep = "\n"
  )
}

#' Get tool schemas for structured LLM tool calling
#'
#' Returns a list of tool definitions describing available sdcMicro
#' anonymization operations. Used for both native tool calling APIs
#' and JSON-based fallback prompts.
#'
#' @return A list of tool schema definitions.
#' @keywords internal
get_tool_schemas <- function() {
  list(
    list(
      name = "groupAndRename",
      description = "Recode a categorical key variable by merging factor levels. The 'before' vector MUST contain ALL existing levels of the variable.",
      parameters = list(
        var = list(type = "string", description = "Name of the key variable"),
        before = list(type = "array", items = list(type = "string"), description = "ALL current levels"),
        after = list(type = "array", items = list(type = "string"), description = "New levels (same length as before)")
      ),
      required = c("var", "before", "after")
    ),
    list(
      name = "localSuppression",
      description = "Enforce k-anonymity by suppressing (setting to NA) the minimum number of key variable values.",
      parameters = list(
        k = list(type = "integer", description = "Desired k-anonymity level")
      ),
      required = c("k")
    ),
    list(
      name = "microaggregation",
      description = "Apply microaggregation to numerical variables.",
      parameters = list(
        variables = list(type = "array", items = list(type = "string"), description = "Numerical variable names"),
        method = list(type = "string", description = "Method: 'mdav' (< 20000 rows) or 'onedims' (larger)")
      ),
      required = c("variables", "method")
    ),
    list(
      name = "addNoise",
      description = "Add noise to numerical variables.",
      parameters = list(
        variables = list(type = "array", items = list(type = "string"), description = "Numerical variable names"),
        noise = list(type = "number", description = "Noise level (default 150)")
      ),
      required = c("variables")
    ),
    list(
      name = "pram",
      description = "Apply Post-Randomization Method to PRAM variables defined in the sdcObj.",
      parameters = list(),
      required = character(0)
    ),
    list(
      name = "topBotCoding",
      description = "Apply top or bottom coding to a numerical variable.",
      parameters = list(
        column = list(type = "string", description = "Variable name"),
        value = list(type = "number", description = "Threshold value"),
        replacement = list(type = "number", description = "Replacement value"),
        kind = list(type = "string", description = "'top' or 'bottom'")
      ),
      required = c("column", "value", "replacement", "kind")
    )
  )
}

#' Execute structured tool calls on an sdcMicroObj
#'
#' Validates and executes a list of tool calls returned by the LLM.
#' Each call is a list with at minimum a \code{tool} field naming the
#' sdcMicro function, plus the parameters for that function.
#'
#' @param sdcObj An sdcMicroObj.
#' @param calls A list of tool call lists, each with a \code{tool} field.
#' @return Modified sdcMicroObj.
#' @keywords internal
execute_tool_calls <- function(sdcObj, calls) {
  dat_names <- colnames(get.sdcMicroObj(sdcObj, type = "origData"))
  key_names <- dat_names[get.sdcMicroObj(sdcObj, type = "keyVars")]
  num_names <- dat_names[get.sdcMicroObj(sdcObj, type = "numVars")]

  for (call in calls) {
    tool <- call$tool
    # Strip namespace prefixes (e.g. "functions.groupAndRename" -> "groupAndRename")
    tool <- sub("^.*\\.", "", tool)
    switch(tool,
      "groupAndRename" = {
        if (!call$var %in% key_names)
          stop(sprintf("Variable '%s' not found in keyVars.", call$var), call. = FALSE)
        # Ensure the column is a factor (keyVars may be stored as integer)
        mk <- get.sdcMicroObj(sdcObj, type = "manipKeyVars")
        if (!is.factor(mk[[call$var]])) {
          mk[[call$var]] <- as.factor(mk[[call$var]])
          sdcObj <- set.sdcMicroObj(sdcObj, type = "manipKeyVars", input = list(mk))
        }
        # Unlist JSON arrays to character vectors
        before <- as.character(unlist(call$before))
        after <- as.character(unlist(call$after))
        sdcObj <- groupAndRename(sdcObj,
          var = call$var,
          before = before,
          after = after)
      },
      "localSuppression" = {
        k <- if (!is.null(call$k)) call$k else 3L
        sdcObj <- localSuppression(sdcObj, k = k)
      },
      "microaggregation" = {
        variables <- as.character(unlist(call$variables))
        for (v in variables) {
          if (!v %in% num_names)
            stop(sprintf("Variable '%s' not found in numVars.", v), call. = FALSE)
        }
        method <- if (!is.null(call$method)) call$method else "mdav"
        sdcObj <- microaggregation(sdcObj, variables = variables, method = method)
      },
      "addNoise" = {
        variables <- as.character(unlist(call$variables))
        for (v in variables) {
          if (!v %in% num_names)
            stop(sprintf("Variable '%s' not found in numVars.", v), call. = FALSE)
        }
        noise <- if (!is.null(call$noise)) call$noise else 150
        sdcObj <- addNoise(sdcObj, variables = variables, noise = noise)
      },
      "pram" = {
        sdcObj <- pram(sdcObj)
      },
      "topBotCoding" = {
        if (!call$column %in% num_names)
          stop(sprintf("Variable '%s' not found in numVars.", call$column), call. = FALSE)
        sdcObj <- topBotCoding(sdcObj,
          value = call$value,
          replacement = call$replacement,
          kind = call$kind,
          column = call$column)
      },
      stop(sprintf("Unknown tool: '%s'", tool), call. = FALSE)
    )
  }
  sdcObj
}

#' Combined utility score for anonymization quality
#'
#' Computes a weighted score from suppression rate (NAs from localSuppression),
#' category loss (levels merged by groupAndRename), and numerical information
#' loss (IL1). Lower is better; 0 means no information loss.
#'
#' @param sdcObj_orig Original sdcMicroObj (before anonymization).
#' @param sdcObj_anon Anonymized sdcMicroObj (after anonymization).
#' @param weights Numeric vector of length 3: weights for suppression rate,
#'   category loss, and IL1. Default \code{c(1/3, 1/3, 1/3)}.
#' @return A list with components: \code{total}, \code{suppression_rate},
#'   \code{category_loss}, \code{il1}, and \code{weights}.
#' @keywords internal
ai_utility_score <- function(sdcObj_orig, sdcObj_anon, weights = c(1/3, 1/3, 1/3)) {
  stopifnot(length(weights) == 3)
  weights <- weights / sum(weights)

  # --- Suppression rate ---
  orig_key <- get.sdcMicroObj(sdcObj_orig, type = "manipKeyVars")
  anon_key <- get.sdcMicroObj(sdcObj_anon, type = "manipKeyVars")
  orig_nas <- sum(is.na(as.matrix(orig_key)))
  anon_nas <- sum(is.na(as.matrix(anon_key)))
  new_nas <- max(0, anon_nas - orig_nas)
  total_cells <- nrow(orig_key) * ncol(orig_key)
  suppression_rate <- if (total_cells > 0) new_nas / total_cells else 0

  # --- Category loss ---
  orig_manip <- get.sdcMicroObj(sdcObj_orig, type = "manipKeyVars")
  anon_manip <- get.sdcMicroObj(sdcObj_anon, type = "manipKeyVars")
  cat_losses <- vapply(seq_len(ncol(orig_manip)), function(i) {
    # Only compare on rows where the anon value is not NA, so that
    # suppressions (NA) do not inflate category loss.
    non_na <- !is.na(anon_manip[[i]])
    orig_levels <- length(unique(orig_manip[[i]][non_na]))
    anon_levels <- length(unique(anon_manip[[i]][non_na]))
    if (orig_levels > 0) 1 - anon_levels / orig_levels else 0
  }, numeric(1))
  category_loss <- mean(cat_losses)

  # --- Numerical IL1 ---
  num_idx <- get.sdcMicroObj(sdcObj_orig, type = "numVars")
  il1 <- 0
  if (length(num_idx) > 0) {
    orig_num <- get.sdcMicroObj(sdcObj_orig, type = "origData")[, num_idx, drop = FALSE]
    anon_num <- get.sdcMicroObj(sdcObj_anon, type = "manipNumVars")
    if (!is.null(anon_num) && ncol(anon_num) > 0) {
      raw_il1 <- dUtility(obj = orig_num, xm = anon_num, method = "IL1s")
      il1 <- raw_il1 / (nrow(orig_num) * ncol(orig_num))
    }
  }

  total <- weights[1] * suppression_rate + weights[2] * category_loss + weights[3] * il1

  list(
    total = total,
    suppression_rate = suppression_rate,
    category_loss = category_loss,
    il1 = il1,
    weights = weights
  )
}

