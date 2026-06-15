# AI-assisted anonymization — examples and revision experiments

Companion material for the SoftwareX paper *"sdcMicro meets GPT: an agentic loop for
privacy-preserving microdata anonymization"* (manuscript SOFTX-D-26-00436). Everything here uses
the `testdata` dataset bundled with `sdcMicro` (>= 5.8.2), so it is fully reproducible.

## Contents

| File | Purpose |
|---|---|
| `example_transcript.txt` | Console transcript of the paper's illustrative session (GPT-4.1, `temperature = 0`) |
| `_setup.R` | Shared helpers for the experiments (backends, sdc object, retry/throttle) |
| `run_ablation.R` | Ablation of the paper: batch-3 / best-of-5 / loop, GPT-4.1 (R=30, k in {3,5,10}) and GPT-5.5 (R=20, k in {3,10}) |
| `run_policy.R` | Empirical check of the `policy` argument (R=20 per level; null result reported in the paper) |
| `analyze.R` | **Deterministic** reproduction of the paper's Table 3 and all reported tests from the shipped CSVs — no API key needed |
| `results/*.csv` | Raw per-run results backing Table 3 and the reported statistics |

## Reproducing the paper's numbers (no LLM access required)

```r
source("analyze.R")
```

reads `results/*.csv` and prints the Table-3 cell means, the Welch tests
(loop vs. batch-3 and loop vs. best-of-5 per model and k), early-stopping rates, timing
summaries, and the policy-experiment summary.

## Re-running the experiments (LLM access required)

```sh
Rscript run_ablation.R gpt41 full      # OPENAI_API_KEY required
Rscript run_ablation.R gpt5  compact
Rscript run_policy.R   gpt41 full
```

Notes:
- LLM output is stochastic even at `temperature = 0`; re-runs reproduce the qualitative
  pattern, not identical numbers. The paper's exact numbers come from the shipped CSVs.
- GPT-5.x models do not accept a custom temperature; `query_llm()` handles this automatically.

## Reading the CSVs

Columns: `backend, model, k, condition, rep, secs, U, S, C, IL1, k_viol, n_refine, early_stop,
error`. `n_refine` counts captured "Refinement iteration" console messages and `secs` is total
wall-clock per call; both accumulate across automatic retry attempts on transient API errors
(`with_retry` in `_setup.R`), so a retried run can show `n_refine` exceeding `max_iter` and
inflated `secs`. Rows with `error=TRUE` carry no outcome and are excluded from all analyses
(4 of 120 GPT-5.5 runs).
