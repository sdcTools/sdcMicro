#' Create an sdcMicro Object with LLM Assistance
#'
#' This function uses a Large Language Model (LLM) to automatically classify variables
#' in a dataset into quasi-identifiers, sensitive variables, numerical variables, and more,
#' and passes the result to `createSdcObj()`. It optionally uses a codebook and policy context.
#'
#' @param dat A data.frame containing the microdata.
#' @param codebook Optional path to a codebook file (currently not parsed; placeholder for future use).
#' @param policy Data sharing policy context: `"open"` (default), `"restricted"`, or `"confidential"`.
#' @param model The LLM model to use (default: `"gpt-4"`).
#' @param api_key OpenAI API key, defaulting to the `OPENAI_API_KEY` environment variable.
#' @param ... Additional arguments passed to `createSdcObj()`.
#'
#' @return An object of class `sdcMicroObj`.
#' @author Matthias Templ
#' @export
#'
#' @examples
#' \dontrun{
#' data(testdata)
#' sdc <- KI_createSdcObj(dat = testdata, policy = "open")
#' sdc
#' }
KI_createSdcObj <- function(dat,
                            codebook = NULL,
                            policy = c("open", "restricted", "confidential"),
                            model = "gpt-4",
                            api_key = Sys.getenv("OPENAI_API_KEY"),
                            ...) {
  policy <- match.arg(policy)
  meta <- extract_variable_metadata(dat, codebook)
  prompt <- build_llm_prompt(meta, policy)
  roles <- query_llm_for_roles(prompt, model = model, api_key = api_key)
  
  message("LLM classification result:\n")
  print(roles)
  
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