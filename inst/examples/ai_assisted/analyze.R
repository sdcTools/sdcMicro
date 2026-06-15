## Deterministic reproduction of the SoftwareX paper's Table 3 and all reported
## statistics from the shipped result files. No API key, no LLM calls.
##
## Usage:  Rscript analyze.R     (from this directory)

rd <- file.path(getwd(), "results")
g41 <- read.csv(file.path(rd, "ablation_gpt41_full.csv"))
g55 <- read.csv(file.path(rd, "ablation_gpt5_compact.csv"))
pol <- read.csv(file.path(rd, "policy_gpt41_full.csv"))

g41 <- g41[!g41$error, ]; g55 <- g55[!g55$error, ]; pol <- pol[!pol$error, ]

m <- function(d, kk, cc) mean(d$U[d$k == kk & d$condition == cc])
pv <- function(d, kk, a, b)
  t.test(d$U[d$k == kk & d$condition == a], d$U[d$k == kk & d$condition == b])$p.value

cat("== Table 3: mean information loss U (lower is better) ==\n")
tab <- rbind(
  data.frame(model = "GPT-4.1", k = c(3, 10),
             batch3 = c(m(g41,3,"batch3"), m(g41,10,"batch3")),
             best_of_5 = c(m(g41,3,"batch5"), m(g41,10,"batch5")),
             loop = c(m(g41,3,"loop"), m(g41,10,"loop"))),
  data.frame(model = "GPT-5.5", k = c(3, 10),
             batch3 = c(m(g55,3,"batch3"), m(g55,10,"batch3")),
             best_of_5 = c(m(g55,3,"batch5"), m(g55,10,"batch5")),
             loop = c(m(g55,3,"loop"), m(g55,10,"loop"))))
tab[, 3:5] <- round(tab[, 3:5], 4)
print(tab, row.names = FALSE)

cat("\n== Welch tests reported in the paper/letter ==\n")
cat(sprintf("GPT-4.1 loop vs batch-3 (k=3/5/10):   p = %.3f / %.3f / %.3f  [no improvement]\n",
            pv(g41,3,"loop","batch3"), pv(g41,5,"loop","batch3"), pv(g41,10,"loop","batch3")))
cat(sprintf("GPT-4.1 loop vs best-of-5 at k=10:    p = %.3f  [best-of-5 better]\n",
            pv(g41,10,"loop","batch5")))
cat(sprintf("GPT-5.5 loop vs batch-3 (k=3, k=10):  p = %.2g / %.3f\n",
            pv(g55,3,"loop","batch3"), pv(g55,10,"loop","batch3")))
cat(sprintf("GPT-5.5 loop vs best-of-5 at k=3:     p = %.2g  (-%.0f%%)\n",
            pv(g55,3,"loop","batch5"),
            100*(m(g55,3,"batch5")-m(g55,3,"loop"))/m(g55,3,"batch5")))
cat(sprintf("GPT-5.5 loop vs best-of-5 at k=10:    p = %.2f  [not significant]\n",
            pv(g55,10,"loop","batch5")))

cat("\n== Early stopping and timing ==\n")
l41 <- g41[g41$condition == "loop", ]; l55 <- g55[g55$condition == "loop", ]
cat(sprintf("GPT-4.1 loop: n=%d, early-stop rate=%.0f%%, n_refine: %s, mean wall-clock=%.1f s\n",
            nrow(l41), 100*mean(l41$early_stop),
            paste(names(table(l41$n_refine)), table(l41$n_refine), sep = "x", collapse = ", "),
            mean(l41$secs)))
cat(sprintf("GPT-5.5 loop: n=%d, early-stop rate=%.0f%% (k=3) / %.0f%% (k=10)\n",
            nrow(l55), 100*mean(l55$early_stop[l55$k == 3]),
            100*mean(l55$early_stop[l55$k == 10])))

cat("\n== Policy experiment (paper: claim softened) ==\n")
print(aggregate(n_key ~ policy, pol, function(x) round(mean(x), 2)), row.names = FALSE)
cat("Distinct proposed key sets:", length(unique(pol$keyvars)), "\n")
