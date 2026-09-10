#' Local suppression by hitting minimum unsafe combinations (de Waal 1998)
#'
#' The historical baseline, reimplemented exactly: de Waal & Willenborg's
#' optimal local suppression (JOS 1998, section 4; dissertation 2003, Ch. 17,
#' models (17.1)-(17.5)). Safety there is *static*: a record is deemed safe
#' once at least one value of each of its **minimum unsafe combinations**
#' (MINUCs) is suppressed, with all frequencies counted on the **original**
#' file -- suppressions never change anyone's count. The model decomposes per
#' record into a tiny weighted hitting-set problem, solved here exactly by
#' enumeration (de Waal's own remark that complete enumeration is practical).
#'
#' The certificate is *sufficient but never necessary* for wildcard
#' k-anonymity: a record whose remaining combination was frequent in the
#' original file is S1-safe in the released file, so
#' `ls_minuc()$objective >= ls_optimal()$objective` on every instance. The
#' measured slack is the price of the 1998 assumption (see
#' `docs/06-literature.md`), e.g. de Waal pays 3 where the exact wildcard
#' optimum is 2 on the smallest witness.
#'
#' Frequencies of key combinations treat `NA` as a category of its own
#' (the era's complete-data convention; matches "extended match").
#'
#' @param x A data.frame of microdata.
#' @param keyVars Key variables, as column names or indices.
#' @param k Required minimum frequency for every key combination.
#' @param importance Optional integer vector as in [ls_optimal()]:
#'   1 = most important = most expensive to suppress.
#' @return A list with `xAnon`, `objective` (weighted cost), `nsupp`,
#'   `records` (row indices that had MINUCs) and `minucs` (their MINUCs as
#'   lists of key-index vectors).
#' @seealso [ls_optimal()] for the exact criterion this baseline bounds.
#' @export
ls_minuc <- function(x, keyVars, k = 2, importance = NULL) {
  stopifnot(is.data.frame(x), k >= 1L)
  key <- as.data.frame(x[, keyVars, drop = FALSE])
  key[] <- lapply(key, as.character)
  n <- nrow(key)
  p <- ncol(key)
  stopifnot(p <= 16L)

  cost_j <- if (is.null(importance)) rep(1, p) else {
    stopifnot(length(importance) == p)
    p + 1 - as.numeric(importance)
  }

  ## ---- marginal counts for every nonempty key subset ----------------------
  ## unsafe(i, S) <=> count of records sharing i's values on S is < k.
  ## Counts shrink as S grows, so "unsafe" is upward closed and every unsafe
  ## subset contains a minimal one.
  dt <- data.table::as.data.table(key)
  data.table::setnames(dt, paste0("K", seq_len(p)))
  for (j in seq_len(p)) {
    col <- paste0("K", j)
    dt[is.na(get(col)), (col) := ".NA."]     # NA as a category of its own
  }
  masks <- seq_len(2L^p - 1L)
  unsafe <- matrix(FALSE, n, length(masks))
  for (s in masks) {
    cols <- paste0("K", which(bitwAnd(s, 2L^(seq_len(p) - 1L)) > 0L))
    cnt <- dt[, .ls_n := .N, by = cols][[".ls_n"]]
    unsafe[, s] <- cnt < k
  }
  dt[, ".ls_n" := NULL]

  ## ---- minimal unsafe combinations per record -----------------------------
  ord <- masks[order(vapply(masks, function(s) sum(bitwAnd(s, 2L^(0:(p - 1))) > 0L),
                            numeric(1)))]
  records <- which(rowSums(unsafe) > 0L)
  minucs <- vector("list", length(records))
  names(minucs) <- as.character(records)
  for (r in seq_along(records)) {
    i <- records[r]
    mins <- integer(0)
    for (s in ord) {
      if (!unsafe[i, s]) next
      if (any(bitwAnd(mins, s) == mins)) next   # a kept minimal is a subset
      mins <- c(mins, s)
    }
    minucs[[r]] <- mins
  }

  ## ---- per-record minimum-cost hitting pattern ----------------------------
  pat_masks <- 0:(2L^p - 1L)
  bitmat <- vapply(seq_len(p), function(b) bitwAnd(pat_masks, 2L^(b - 1L)) > 0L,
                   logical(length(pat_masks)))
  pat_cost <- as.numeric(matrix(bitmat, nrow = length(pat_masks)) %*% cost_j)

  xAnon <- x
  objective <- 0
  nsupp <- 0L
  for (r in seq_along(records)) {
    i <- records[r]
    feas <- rep(TRUE, length(pat_masks))
    for (mk in minucs[[r]]) feas <- feas & (bitwAnd(pat_masks, mk) > 0L)
    best <- which(feas)[which.min(pat_cost[feas])]
    hit <- which(bitwAnd(pat_masks[best], 2L^(seq_len(p) - 1L)) > 0L)
    for (j in hit) {
      if (!is.na(xAnon[i, keyVars[j]])) {
        xAnon[i, keyVars[j]] <- NA
        nsupp <- nsupp + 1L
      }
    }
    objective <- objective + pat_cost[best]
  }

  list(xAnon = xAnon, objective = objective, nsupp = nsupp,
       records = records,
       minucs = lapply(minucs, function(v)
         lapply(v, function(s) which(bitwAnd(s, 2L^(seq_len(p) - 1L)) > 0L))))
}
