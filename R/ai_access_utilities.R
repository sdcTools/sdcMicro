#' Extract Metadata from a Dataset
#'
#' Extracts variable names, types, and cardinality from a data frame, optionally integrating a codebook.
#'
#' @param dat A data.frame.
#' @param codebook Optional path to a codebook file (.csv, .tsv, .json, .yaml, .txt, or .pdf)
#' @author Matthias Templ
#'
#' @return A data.frame with columns: `varname`, `class`, and `n_unique`.
#' @keywords internal
extract_variable_metadata <- function(dat, codebook = NULL) {
  meta <- data.frame(
    varname = names(dat),
    class = sapply(dat, function(x) class(x)[1]),
    n_unique = sapply(dat, function(x) length(unique(x))),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(codebook)) {
    cb_text <- read_codebook_snippet(codebook)
    attr(meta, "codebook") <- cb_text
  }
  
  return(meta)
}

#' Build LLM Prompt for Variable Role Classification
#'
#' Constructs a prompt for the LLM using metadata and optional codebook and data-sharing context.
#'
#' @author Matthias Templ
#' @param meta A data.frame with variable metadata (name, class, cardinality).
#' @param policy One of `"open"`, `"restricted"`, or `"confidential"`.
#' @param codebook Optional path to a CSV, TSV, YAML, JSON, TXT, or PDF file describing variable definitions.
#'
#' @return A character string prompt.
#' @keywords internal
build_llm_prompt <- function(meta, policy, codebook = NULL) {
  # Basic variable metadata
  tab <- paste0(
    apply(meta, 1, function(row) {
      sprintf("Variable: %s | Type: %s | Unique Values: %s",
              row["varname"], row["class"], row["n_unique"])
    }),
    collapse = "\n"
  )
  
  # Optional codebook summary
  codebook_info <- ""
  if (!is.null(codebook)) {
    codebook_info <- read_codebook_snippet(codebook)
  }
  
  # Final prompt
  prompt <- paste(c(
    "I need help classifying variables in a dataset for anonymization using the sdcMicro package in R.",
    sprintf("The data sharing policy is: %s.", policy),
    "Below is metadata about the dataset variables:",
    tab,
    codebook_info,
    "",
    "\nPlease output only a **valid JSON object** with the following fields: ",
    "`reasoning`, `keyVars`, `pramVars`, `numVars`, `weightVar`, `strataVar`, and `hhId`.",
    "The `reasoning` field should briefly explain why each variable was assigned its role.",
    "Use an empty array ([]) or null where appropriate. Do not include any explanation outside the JSON.",
    "When a variable is selected as keyVars or numVars, it should not in the same time serve as pramVars",
    "When a variable is selected as pramVars, it should not in the same time serve as keyVars or numVars, thus if you want to use a variable for swapping with PRAM, it should not be selected as keyVars or numVars.",
    "Typically, detailed geographical information, e.g. ZIP code or municipality are set as variables for swapping with PRAM.",
    "In case there is only a few keyVars, there might be no need to select any pramVars.",
    "In case of many keyVars with high cardinality, it might be necessary to select some pramVars.",
    "Only samples collected with a complex survey design, or samples that are post-calibrated contains sampling weights, thus in most cases weightVar should be null.",
    "A cluster structure is only present in data like persons within households or pupils in schools, or employees in enterprises, thus in most cases hhId should be null.",
    "Select only one variable as sampling weights (weightVar) or cluster id (hhId).",
    "keyVars are categorical variables that describe individuals and can be used to re-identify them (quasi-identifiers)."
    ), collapse = "\n")
}

#' Read and summarize a codebook file for LLM prompt
#'
#' @param path Path to a codebook file (.csv, .json, .yaml, .txt, .pdf).
#'
#' @author Matthias Templ
#' @return A character string summarizing the file content.
#' @importFrom utils head read.csv str
#' @keywords internal
read_codebook_snippet <- function(path) {
  if (!file.exists(path)) {
    warning("Codebook file not found: ", path)
    return("")
  }
  
  ext <- tolower(tools::file_ext(path))
  out <- NULL
  
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for this feature.", call. = FALSE)
  }
  
  if (ext %in% c("csv", "tsv")) {
    out <- tryCatch({
      cb <- read.csv(path, stringsAsFactors = FALSE)
      preview <- utils::capture.output(print(head(cb, 10)))
      paste("\nHere is additional codebook information (CSV preview):\n",
            paste(preview, collapse = "\n"))
    }, error = function(e) NULL)
  }
  
  else if (ext %in% c("yaml", "yml")) {
    if (!requireNamespace("yaml", quietly = TRUE)) return("")
    out <- tryCatch({
      cb <- yaml::read_yaml(path)
      preview <- utils::capture.output(str(cb, max.level = 2))
      paste("\nHere is additional codebook information (YAML structure):\n",
            paste(preview, collapse = "\n"))
    }, error = function(e) NULL)
  }
  
  else if (ext == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) return("")
    out <- tryCatch({
      cb <- jsonlite::fromJSON(path)
      preview <- utils::capture.output(str(cb, max.level = 2))
      paste("\nHere is additional codebook information (JSON structure):\n",
            paste(preview, collapse = "\n"))
    }, error = function(e) NULL)
  }
  
  else if (ext %in% c("txt", "text")) {
    out <- tryCatch({
      cb <- readLines(path, n = 50)
      paste("\nHere is additional codebook information (ASCII preview):\n",
            paste(cb, collapse = "\n"))
    }, error = function(e) NULL)
  }
  
  else if (ext == "pdf") {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("Package 'pdftools' is required for this feature. Install it with install.packages('pdftools').",
           call. = FALSE)
    }
    out <- tryCatch({
      cb <- pdftools::pdf_text(path)
      preview <- unlist(strsplit(cb[1:min(2, length(cb))], "\n"))[1:50]
      paste("\nHere is additional codebook information (extracted from PDF):\n",
            paste(preview, collapse = "\n"))
    }, error = function(e) NULL)
  }
  
  if (is.null(out)) {
    warning("Could not parse codebook file: ", path)
    return("")
  }
  
  return(out)
}


#' Convert tool schemas to OpenAI function calling format
#' @param tools List of tool schemas from \code{get_tool_schemas()}.
#' @return List suitable for the \code{tools} field in an OpenAI API request body.
#' @noRd
format_tools_openai <- function(tools) {
  lapply(tools, function(tool) {
    properties <- lapply(tool$parameters, function(p) { p })
    # Ensure empty lists serialize as JSON objects {} not arrays []
    if (length(properties) == 0L) {
      properties <- structure(list(), names = character(0))
    }
    params <- list(type = "object", properties = properties)
    if (length(tool$required) > 0L) {
      params$required <- as.list(tool$required)
    }
    list(
      type = "function",
      `function` = list(
        name = tool$name,
        description = tool$description,
        parameters = params
      )
    )
  })
}

#' Convert tool schemas to Anthropic tool_use format
#' @param tools List of tool schemas from \code{get_tool_schemas()}.
#' @return List suitable for the \code{tools} field in an Anthropic API request body.
#' @noRd
format_tools_anthropic <- function(tools) {
  lapply(tools, function(tool) {
    properties <- lapply(tool$parameters, function(p) { p })
    if (length(properties) == 0L) {
      properties <- structure(list(), names = character(0))
    }
    schema <- list(type = "object", properties = properties)
    if (length(tool$required) > 0L) {
      schema$required <- as.list(tool$required)
    }
    list(
      name = tool$name,
      description = tool$description,
      input_schema = schema
    )
  })
}

#' Format tool schemas as text for custom/fallback providers
#' @param tools List of tool schemas from \code{get_tool_schemas()}.
#' @return Character string describing available tools and expected return format.
#' @noRd
format_tools_as_text <- function(tools) {
  tool_lines <- vapply(tools, function(tool) {
    param_names <- names(tool$parameters)
    params_str <- if (length(param_names) > 0) {
      paste(param_names, collapse = ", ")
    } else {
      ""
    }
    sprintf("- %s(%s): %s", tool$name, params_str, tool$description)
  }, character(1))

  paste(
    "\n\nAvailable tools (return a JSON array of tool calls):",
    paste(tool_lines, collapse = "\n"),
    "",
    'Return format: [{"tool": "toolName", "param1": value1, ...}, ...]',
    sep = "\n"
  )
}

#' Parse tool calls from an OpenAI API response
#' @param response Parsed response from \code{httr::content()}.
#' @return A list with \code{$tool_calls} (list of normalized tool calls) and
#'   \code{$content} (text content or NULL).
#' @noRd
parse_tool_calls_openai <- function(response) {
  message <- response$choices[[1]]$message
  text_content <- message$content  # may be NULL

  tool_calls_raw <- message$tool_calls
  parsed_calls <- list()

  if (!is.null(tool_calls_raw) && length(tool_calls_raw) > 0) {
    parsed_calls <- lapply(tool_calls_raw, function(tc) {
      fn_name <- tc$`function`$name
      fn_args <- jsonlite::fromJSON(tc$`function`$arguments, simplifyVector = TRUE)
      c(list(tool = fn_name), as.list(fn_args))
    })
  }

  list(
    tool_calls = parsed_calls,
    content = text_content
  )
}

#' Parse tool calls from an Anthropic API response
#' @param response Parsed response from \code{httr::content()}.
#' @return A list with \code{$tool_calls} (list of normalized tool calls) and
#'   \code{$content} (text content or NULL).
#' @noRd
parse_tool_calls_anthropic <- function(response) {
  content_blocks <- response$content
  text_content <- NULL
  parsed_calls <- list()

  for (block in content_blocks) {
    if (!is.null(block$type) && block$type == "text") {
      text_content <- paste(c(text_content, block$text), collapse = "\n")
    } else if (!is.null(block$type) && block$type == "tool_use") {
      call_entry <- c(list(tool = block$name), as.list(block$input))
      parsed_calls <- c(parsed_calls, list(call_entry))
    }
  }

  list(
    tool_calls = parsed_calls,
    content = text_content
  )
}

#' Parse tool calls from a plain text LLM response (JSON fallback)
#' @param content Character string containing the LLM response text.
#' @return A list with \code{$tool_calls} (list of normalized tool calls) and
#'   \code{$content} (the raw text content).
#' @noRd
parse_tool_calls_text <- function(content) {
  # Try to extract a JSON array from the response
  # Strip markdown code fences if present
  cleaned <- gsub("```json\\s*", "", content)
  cleaned <- gsub("```\\s*", "", cleaned)
  cleaned <- trimws(cleaned)

  # Try to find a JSON array in the text
  json_match <- regmatches(cleaned, regexpr("\\[\\s*\\{.*\\}\\s*\\]", cleaned, perl = TRUE))

  parsed_calls <- list()

  if (length(json_match) > 0) {
    parsed <- tryCatch(
      jsonlite::fromJSON(json_match[1], simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.list(parsed)) {
      parsed_calls <- lapply(parsed, function(entry) {
        # Normalize: ensure 'tool' field exists
        as.list(entry)
      })
    }
  }

  list(
    tool_calls = parsed_calls,
    content = content
  )
}

#' Query an LLM (provider-agnostic)
#'
#' Sends a prompt to an LLM and returns the response content as a string.
#' Supports OpenAI, Anthropic, and any OpenAI-compatible endpoint (Ollama, Azure, vLLM, Groq, Together, etc.).
#' When \code{tools} is provided, uses native tool calling (OpenAI/Anthropic) or JSON fallback (custom).
#'
#' @param prompt Character string with the user message.
#' @param system_prompt Character string with the system message.
#' @param provider One of \code{"openai"}, \code{"anthropic"}, or \code{"custom"}.
#'   \code{"custom"} uses the OpenAI-compatible format with a custom \code{base_url}.
#' @param model Model identifier. If \code{NULL}, defaults to \code{"gpt-4.1"} for OpenAI,
#'   \code{"claude-sonnet-4-20250514"} for Anthropic.
#' @param api_key API key. If \code{NULL}, auto-detected from environment variables:
#'   \code{OPENAI_API_KEY}, \code{ANTHROPIC_API_KEY}, or \code{LLM_API_KEY}.
#' @param base_url Base URL for the API. Defaults per provider; required for \code{"custom"}.
#' @param temperature Sampling temperature (default 0 for deterministic output).
#' @param tools Optional list of tool schemas (from \code{get_tool_schemas()}). When provided,
#'   enables tool calling and returns a structured list instead of a string.
#'
#' @return When \code{tools = NULL}, the LLM response content as a character string.
#'   When \code{tools} is provided, a list with \code{$tool_calls} (list of normalized
#'   tool call lists, each with \code{$tool} and parameters) and \code{$content}
#'   (text content or NULL).
#' @keywords internal
query_llm <- function(prompt,
                      system_prompt = "You are a data anonymization assistant.",
                      provider = c("openai", "anthropic", "custom"),
                      model = NULL,
                      api_key = NULL,
                      base_url = NULL,
                      temperature = 0,
                      tools = NULL) {
  provider <- match.arg(provider)

  # --- Resolve API key ---
  if (is.null(api_key) || api_key == "") {
    env_vars <- switch(provider,
      openai    = c("OPENAI_API_KEY", "LLM_API_KEY"),
      anthropic = c("ANTHROPIC_API_KEY", "LLM_API_KEY"),
      custom    = c("LLM_API_KEY", "OPENAI_API_KEY")
    )
    for (ev in env_vars) {
      api_key <- Sys.getenv(ev, "")
      if (nzchar(api_key)) break
    }
  }
  if (is.null(api_key) || !nzchar(api_key)) {
    if (interactive()) {
      api_key <- readline(prompt = sprintf("Enter your %s API key: ", provider))
      if (!nzchar(api_key)) stop("No API key provided.", call. = FALSE)
    } else {
      stop("No API key found. Set the appropriate environment variable or pass api_key.", call. = FALSE)
    }
  }

  # --- Resolve model ---
  if (is.null(model)) {
    model <- switch(provider,
      openai    = "gpt-4.1",
      anthropic = "claude-sonnet-4-20250514",
      custom    = "gpt-4.1"
    )
  }

  # --- Resolve base URL ---
  if (is.null(base_url)) {
    base_url <- switch(provider,
      openai    = "https://api.openai.com/v1",
      anthropic = "https://api.anthropic.com/v1",
      custom    = stop("base_url is required when provider = 'custom'.", call. = FALSE)
    )
  }
  # Remove trailing slash
  base_url <- sub("/$", "", base_url)

  # --- For custom provider with tools, use text fallback ---
  use_text_fallback <- !is.null(tools) && provider == "custom"
  effective_prompt <- prompt
  if (use_text_fallback) {
    effective_prompt <- paste0(prompt, format_tools_as_text(tools))
  }

  # --- Send request ---
  if (provider == "anthropic") {
    req_body <- list(
      model = model,
      max_tokens = 4096L,
      system = system_prompt,
      messages = list(list(role = "user", content = effective_prompt)),
      temperature = temperature
    )
    # Add tools for Anthropic native tool calling
    if (!is.null(tools) && !use_text_fallback) {
      req_body$tools <- format_tools_anthropic(tools)
    }
    res <- httr::POST(
      url = paste0(base_url, "/messages"),
      httr::add_headers(
        `x-api-key` = api_key,
        `anthropic-version` = "2023-06-01",
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(req_body, auto_unbox = TRUE)
    )
    res_content <- httr::content(res, as = "parsed")
    if (!is.null(res_content$error)) {
      stop("Anthropic API error: ", res_content$error$message, call. = FALSE)
    }

    # Return structured result if tools were requested
    if (!is.null(tools)) {
      return(parse_tool_calls_anthropic(res_content))
    }

    content <- res_content$content[[1]]$text
  } else {
    # OpenAI and OpenAI-compatible (custom)
    req_body <- list(
      model = model,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = effective_prompt)
      ),
      temperature = temperature
    )
    # Add tools for OpenAI native tool calling (not for custom/text fallback)
    if (!is.null(tools) && !use_text_fallback) {
      req_body$tools <- format_tools_openai(tools)
    }
    res <- httr::POST(
      url = paste0(base_url, "/chat/completions"),
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(req_body, auto_unbox = TRUE)
    )
    res_content <- httr::content(res, as = "parsed")
    if (!is.null(res_content$error)) {
      stop("LLM API error: ", res_content$error$message, call. = FALSE)
    }
    if (is.null(res_content$choices) || length(res_content$choices) == 0) {
      stop("LLM returned an empty response.", call. = FALSE)
    }

    # Return structured result if tools were requested
    if (!is.null(tools)) {
      if (use_text_fallback) {
        text_content <- res_content$choices[[1]]$message$content
        return(parse_tool_calls_text(text_content))
      }
      return(parse_tool_calls_openai(res_content))
    }

    content <- res_content$choices[[1]]$message$content
  }

  if (is.null(content) || is.na(content)) {
    stop("LLM returned an invalid response.", call. = FALSE)
  }
  content
}

#' Query LLM to Classify Variable Roles
#'
#' Sends a prompt to an LLM to classify dataset variables into anonymization roles
#' for use with the sdcMicro package.
#'
#' @param prompt A character string prompt for the LLM.
#' @param model Model identifier (passed to \code{\link{query_llm}}).
#' @param api_key API key (passed to \code{\link{query_llm}}).
#' @param provider LLM provider (passed to \code{\link{query_llm}}).
#' @param base_url Base URL (passed to \code{\link{query_llm}}).
#' @author Matthias Templ
#'
#' @return A named list with fields `keyVars`, `pramVars`, `numVars`, `weightVar`, `strataVar`, `hhId`.
#' @keywords internal
query_llm_for_roles <- function(prompt, model = NULL, api_key = NULL,
                                provider = "openai", base_url = NULL) {
  full_content <- query_llm(
    prompt = prompt,
    system_prompt = "You are a data anonymization assistant.",
    provider = provider,
    model = model,
    api_key = api_key,
    base_url = base_url
  )

  # Remove Markdown formatting (```json ... ```)
  full_content <- gsub("^```json\\s*|\\s*```$", "", full_content)

  parsed <- tryCatch(
    jsonlite::fromJSON(full_content),
    error = function(e) {
      stop("Failed to parse JSON from LLM response:\n", e$message, "\nResponse:\n", full_content)
    }
  )

  # Normalize fields
  expected_fields <- c("keyVars", "pramVars", "numVars", "weightVar", "strataVar", "hhId")
  for (f in expected_fields) {
    if (!f %in% names(parsed)) {
      parsed[[f]] <- NULL
    }
    value <- parsed[[f]]
    if (is.list(value) && length(value) == 0) {
      parsed[[f]] <- character(0)
    } else if (is.character(value) && length(value) == 1 && !is.na(value)) {
      parsed[[f]] <- as.character(value)
    }
    if (length(parsed[[f]]) == 0 ||
        (is.character(parsed[[f]]) && length(parsed[[f]]) == 1 && parsed[[f]] == "")) {
      parsed[[f]] <- NULL
    }
  }
  parsed
}