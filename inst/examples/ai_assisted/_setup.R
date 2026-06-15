## Shared setup for the SoftwareX-paper experiments (ablation + policy check).
## Requires sdcMicro >= 5.8.2. Run from this directory; results go to ./results.
##
## Backends: "gpt41" (GPT-4.1, the paper's headline model) and "gpt5" (GPT-5.5).
## Both use the text/JSON tool path (provider = "custom") so the batch phase
## reliably returns n_strategies candidates and the two arms are directly
## comparable. OPENAI_API_KEY must be set.

library(sdcMicro)
if (utils::packageVersion("sdcMicro") < "5.8.2")
  stop("sdcMicro >= 5.8.2 required (early stopping).")

RESULTS_DIR <- file.path(getwd(), "results")
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
WEIGHTS <- c(1/3, 1/3, 1/3)

backend_spec <- function(backend = c("gpt41", "gpt5")) {
  backend <- match.arg(backend)
  switch(backend,
    gpt41 = list(provider = "custom", model = "gpt-4.1",
                 base_url = "https://api.openai.com/v1",
                 api_key  = Sys.getenv("OPENAI_API_KEY")),
    gpt5  = list(provider = "custom", model = "gpt-5.5",
                 base_url = "https://api.openai.com/v1",
                 api_key  = Sys.getenv("OPENAI_API_KEY"))
  )
}

## The paper's fixed configuration: testdata with seven categorical
## quasi-identifiers and three numeric variables.
build_testdata_sdc <- function() {
  data("testdata", package = "sdcMicro", envir = environment())
  createSdcObj(testdata,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
}

k_violations <- function(sdc, k) {
  fk <- tryCatch(sdc@risk$individual[, "fk"], error = function(e) NA_real_)
  if (length(fk) == 0 || all(is.na(fk))) return(NA_integer_)
  as.integer(sum(fk < k))
}

## Robustness for long runs: throttle between calls; retry transient API errors
## with exponential backoff. NOTE: captured console messages (and wall-clock)
## accumulate across retry attempts -- see README "Reading the CSVs".
THROTTLE_SEC <- 1.5
with_retry <- function(f, tries = 4L, base = 6) {
  last <- NULL
  for (a in seq_len(tries)) {
    out <- tryCatch(f(), error = function(e) { last <<- conditionMessage(e); NULL })
    if (!is.null(out)) return(out)
    if (a < tries) Sys.sleep(base * 2^(a - 1))
  }
  stop(if (is.null(last)) "with_retry: failed" else last, call. = FALSE)
}
