## Empirical check of the `policy` argument of AI_createSdcObj(): does
## policy in {open, restricted, confidential} change the proposed
## quasi-identifier set on testdata?  (Paper: no -- the proposed set was
## identical across levels in all runs; the manuscript claim was softened
## accordingly.)
##
## Usage:  Rscript run_policy.R [gpt41|gpt5] [pilot|full]
##   pilot: R=5;  full: R=20 (paper)

source("_setup.R")

args    <- commandArgs(trailingOnly = TRUE)
BACKEND <- if (length(args) >= 1) args[1] else "gpt41"
MODE    <- if (length(args) >= 2) args[2] else "pilot"
spec    <- backend_spec(BACKEND)

R        <- if (MODE == "full") 20L else 5L
POLICIES <- c("open", "restricted", "confidential")

data("testdata", package = "sdcMicro")

run_one <- function(policy, rep) {
  res <- tryCatch(
    suppressMessages(withCallingHandlers(
      with_retry(function() AI_createSdcObj(
        dat = testdata, policy = policy,
        provider = spec$provider, model = spec$model,
        base_url = spec$base_url, api_key = spec$api_key, confirm = FALSE)),
      message = function(m) invokeRestart("muffleMessage"))),
    error = function(e) { message("ERR [", policy, " r", rep, "]: ", conditionMessage(e)); NULL })

  base <- data.frame(backend = BACKEND, model = spec$model, policy = policy, rep = rep,
                     stringsAsFactors = FALSE)
  if (is.null(res)) {
    return(cbind(base, n_key = NA_integer_, n_num = NA_integer_, keyvars = NA_character_,
                 error = TRUE))
  }
  kv <- tryCatch(colnames(res@origData)[res@keyVars], error = function(e) character(0))
  nv <- tryCatch(length(res@numVars), error = function(e) NA_integer_)
  cbind(base, n_key = length(kv), n_num = nv,
        keyvars = paste(sort(kv), collapse = ";"), error = FALSE)
}

grid <- expand.grid(policy = POLICIES, rep = seq_len(R), stringsAsFactors = FALSE)
message(sprintf("Policy: backend=%s mode=%s -> %d runs", BACKEND, MODE, nrow(grid)))

rows <- lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  message(sprintf("[%d/%d] policy=%-12s rep=%d", i, nrow(grid), g$policy, g$rep))
  res <- run_one(g$policy, g$rep); Sys.sleep(THROTTLE_SEC); res
})
out <- do.call(rbind, rows)

f <- file.path(RESULTS_DIR, sprintf("policy_%s_%s", BACKEND, MODE))
saveRDS(out, paste0(f, ".rds")); write.csv(out, paste0(f, ".csv"), row.names = FALSE)

ok <- out[!out$error, ]
if (nrow(ok)) {
  cat("\nMean #key variables by policy:\n")
  print(aggregate(n_key ~ policy, ok, function(x) round(mean(x), 2)))
}
message("Saved to ", RESULTS_DIR)
