## Ablation from the SoftwareX paper: does feedback-conditioned refinement beat
## sampling, and does refinement improve on the initial batch?
##
## Conditions (per (k, rep)):
##   batch3 : n_strategies = 3, max_iter = 0   (no refinement; the loop's start)
##   batch5 : n_strategies = 5, max_iter = 0   (best-of-5, no feedback;
##                                              matched to the loop's max budget)
##   loop   : n_strategies = 3, max_iter = 2   (+ early stopping)
##
## Usage:  Rscript run_ablation.R [gpt41|gpt5] [pilot|compact|full]
##   pilot:   R=5,  k in {3,10}
##   compact: R=20, k in {3,10}      (paper: GPT-5.5 arm)
##   full:    R=30, k in {3,5,10}    (paper: GPT-4.1 arm)

source("_setup.R")

args    <- commandArgs(trailingOnly = TRUE)
BACKEND <- if (length(args) >= 1) args[1] else "gpt41"
MODE    <- if (length(args) >= 2) args[2] else "pilot"
spec    <- backend_spec(BACKEND)

R      <- switch(MODE, full = 30L, compact = 20L, 5L)
K_VALS <- if (MODE == "full") c(3L, 5L, 10L) else c(3L, 10L)
CONDITIONS <- list(
  batch3 = list(n_strategies = 3L, max_iter = 0L),
  batch5 = list(n_strategies = 5L, max_iter = 0L),
  loop   = list(n_strategies = 3L, max_iter = 2L)
)

sdc0 <- build_testdata_sdc()

run_one <- function(k, cond, rep) {
  cfg  <- CONDITIONS[[cond]]
  msgs <- character(0)
  el <- system.time({
    res <- tryCatch(
      withCallingHandlers(
        with_retry(function() AI_applyAnonymization(
          sdc0, k = k,
          n_strategies = cfg$n_strategies, max_iter = cfg$max_iter,
          patience = 1L, tol = 1e-3, weights = WEIGHTS,
          provider = spec$provider, model = spec$model,
          base_url = spec$base_url, api_key = spec$api_key,
          confirm = FALSE, verbose = TRUE, generateReport = FALSE)),
        message = function(m) {
          msgs[[length(msgs) + 1L]] <<- conditionMessage(m); invokeRestart("muffleMessage")
        }),
      error = function(e) {
        msgs[[length(msgs) + 1L]] <<- paste("ERROR:", conditionMessage(e)); NULL
      })
  })["elapsed"]

  base <- data.frame(backend = BACKEND, model = spec$model, k = k, condition = cond,
                     rep = rep, secs = unname(el), stringsAsFactors = FALSE)
  if (is.null(res)) {
    return(cbind(base, U = NA_real_, S = NA_real_, C = NA_real_, IL1 = NA_real_,
                 k_viol = NA_integer_, n_refine = NA_integer_, early_stop = NA,
                 error = TRUE))
  }
  sc  <- sdcMicro:::ai_utility_score(sdc0, res, WEIGHTS)
  txt <- paste(msgs, collapse = "\n")
  n_refine <- length(unlist(regmatches(txt, gregexpr("Refinement iteration", txt))))
  cbind(base,
        U = sc$total, S = sc$suppression_rate, C = sc$category_loss, IL1 = sc$il1,
        k_viol = k_violations(res, k), n_refine = n_refine,
        early_stop = grepl("Early stopping", txt), error = FALSE)
}

grid <- expand.grid(k = K_VALS, condition = names(CONDITIONS), rep = seq_len(R),
                    stringsAsFactors = FALSE)
message(sprintf("Ablation: backend=%s mode=%s -> %d runs", BACKEND, MODE, nrow(grid)))

rows <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  message(sprintf("[%d/%d] k=%d %-6s rep=%d", i, nrow(grid), g$k, g$condition, g$rep))
  rows[[i]] <- run_one(g$k, g$condition, g$rep)
  Sys.sleep(THROTTLE_SEC)
}
out <- do.call(rbind, rows)

f <- file.path(RESULTS_DIR, sprintf("ablation_%s_%s", BACKEND, MODE))
saveRDS(out, paste0(f, ".rds")); write.csv(out, paste0(f, ".csv"), row.names = FALSE)

ok <- out[!out$error, ]
if (nrow(ok)) {
  cat("\nMean U by k x condition (lower is better):\n")
  print(reshape(aggregate(U ~ k + condition, ok, function(x) round(mean(x), 4)),
                idvar = "k", timevar = "condition", direction = "wide"))
  cat("\nError rate:", round(mean(out$error), 3), "\n")
}
message("Saved to ", RESULTS_DIR)
