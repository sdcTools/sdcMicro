#' Extract Metadata from a Dataset
#'
#' Extracts variable names, types, and cardinality from a data frame.
#'
#' @param dat A data.frame.
#' @param codebook Optional path to a codebook file (not used yet).
#' @author Matthias Templ
#'
#' @return A data.frame with columns: `varname`, `class`, and `n_unique`.
#' @keywords internal
extract_variable_metadata <- function(dat, codebook = NULL) {
  data.frame(
    varname = names(dat),
    class = sapply(dat, function(x) class(x)[1]),
    n_unique = sapply(dat, function(x) length(unique(x))),
    stringsAsFactors = FALSE
  )
}

#' Build LLM Prompt for Variable Role Classification
#'
#' Constructs a prompt for the LLM using metadata and the data-sharing context.
#'
#' @param meta A data.frame with variable metadata.
#' @param policy One of `"open"`, `"restricted"`, or `"confidential"`.
#' @author Matthias Templ
#'
#' @return A character string prompt.
#' @keywords internal
build_llm_prompt <- function(meta, policy) {
  tab <- paste0(
    apply(meta, 1, function(row) {
      sprintf("Variable: %s, Type: %s, Unique Values: %s",
              row["varname"], row["class"], row["n_unique"])
    }),
    collapse = "\n"
  )
  paste(
    "I need help classifying variables in a dataset for anonymization using sdcMicro in R.",
    sprintf("The data sharing policy is: %s.", policy),
    "Here are the variables:\n", tab,
    "\nPlease output only a **valid JSON object** with the following fields: ",
    "`keyVars`, `pramVars`, `numVars`, `weightVar`, `strataVar`, and `hhId`.",
    "Use an empty array ([]) or null where appropriate. Do not include any explanation or formatting.",
    "When a variable is selected as keyVars or numVars, it should not in the same time serve as pramVars",
    "When a variable is selected as pramVars, it should not in the same time serve as keyVars or numVars, thus if you want to use a variable for swapping with PRAM, it should not be selected as keyVars or numVars.",
    "Typically, detailed geographical information, e.g. ZIP code or municipality are set as variables for swapping with PRAM.",
    "In case there is only a few keyVars, there might be no need to select any pramVars.",
    "In case of many keyVars with high cardinality, it might be necessary to select some pramVars.",
    "Only samples collected with a complex survey design, or samples that are post-calibrated contains sampling weights, thus in most cases weightVar should be null.",
    "A cluster structure is only present in data like persons within households or pupils in schools, or employees in enterprises, thus in most cases hhId should be null.",
    "Select only one variable as sampling weights (weightVar) or cluster id (hhId)."
  )
}

#' #' Query LLM to Classify Variable Roles
#'
#' Sends a prompt to the OpenAI API to classify dataset variables into anonymization roles
#' for use with the sdcMicro package. If the API key is not provided or set via
#' `Sys.getenv("OPENAI_API_KEY")`, the function will prompt for it interactively.
#'
#' @param prompt A character string prompt for the LLM.
#' @param model The OpenAI model to use (e.g., "gpt-4").
#' @param api_key Optional. If not provided, defaults to environment variable OPENAI_API_KEY.
#' @author Matthias Templ
#'
#' @return A named list with fields `keyVars`, `pramVars`, `numVars`, `weightVar`, `strataVar`, `hhId`.
#' Fields are either character vectors (possibly empty) or NULL.
#' @keywords internal
query_llm_for_roles <- function(prompt, model = "gpt-4", api_key = NULL) {
  # Step 1: Retrieve API key
  if (is.null(api_key) || api_key == "") {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }
  
  if (api_key == "") {
    message("No OpenAI API key found in environment variable 'OPENAI_API_KEY'.")
    api_key <- readline(prompt = "Please enter your OpenAI API key: ")
    if (api_key == "") stop("No API key provided. Aborting.")
    Sys.setenv(OPENAI_API_KEY = api_key)
  }
  
  # Step 2: Send API request
  url <- "https://api.openai.com/v1/chat/completions"
  headers <- c(
    `Authorization` = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = "You are a data anonymization assistant."),
      list(role = "user", content = prompt)
    ),
    temperature = 0
  )
  response <- httr::POST(
    url,
    httr::add_headers(.headers = headers),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )
  
  # Step 3: Extract LLM response content
  full_content <- httr::content(response, as = "parsed")$choices[[1]]$message$content
  
  # Step 4: Remove Markdown formatting (```json ... ```)
  full_content <- gsub("^```json\\s*|\\s*```$", "", full_content)
  
  # Step 5: Parse JSON (full_content is assumed to be valid JSON now)
  parsed <- tryCatch(
    jsonlite::fromJSON(full_content),
    error = function(e) {
      stop("Failed to parse JSON from LLM response:\n", e$message, "\nResponse:\n", full_content)
    }
  )
  
  # Step 6: Normalize fields
  expected_fields <- c("keyVars", "pramVars", "numVars", "weightVar", "strataVar", "hhId")
  for (f in expected_fields) {
    # If field is missing, set to NULL
    if (!f %in% names(parsed)) {
      parsed[[f]] <- NULL
    }
    
    value <- parsed[[f]]
    
    # Convert list() to character(0)
    if (is.list(value) && length(value) == 0) {
      parsed[[f]] <- character(0)
    }
    
    # Convert scalar string to character vector
    else if (is.character(value) && length(value) == 1 && !is.na(value)) {
      parsed[[f]] <- as.character(value)
    }
    
    # Convert atomic(0) or "" to NULL
    if (length(parsed[[f]]) == 0 ||
        (is.character(parsed[[f]]) && length(parsed[[f]]) == 1 && parsed[[f]] == "")) {
      parsed[[f]] <- NULL
    }
  }
  
  return(parsed)
}