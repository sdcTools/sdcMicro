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
#' @param meta A data.frame with variable metadata (name, class, cardinality).
#' @param policy One of `"open"`, `"restricted"`, or `"confidential"`.
#' @param codebook Optional path to a CSV, YAML, or JSON file describing variable definitions.
#'
#' @author Matthias Templ
#' @return A character string prompt.
#' @keywords internal
#' Build LLM Prompt for Variable Role Classification
#'
#' Constructs a prompt for the LLM using metadata, optional codebook, and the data-sharing context.
#'
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
    "`keyVars`, `pramVars`, `numVars`, `weightVar`, `strataVar`, and `hhId`.",
    "Use an empty array ([]) or null where appropriate. Do not include any explanation or formatting.",
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
    if (!requireNamespace("pdftools", quietly = TRUE)) return("")
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