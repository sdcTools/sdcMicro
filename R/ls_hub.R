#' Closed-form hub construction: an O(k p) upper bound on local suppression
#'
#' The constructive reading of the hub family (`docs/08-theory.md` Thm 4.3,
#' bound Thm 4.5). Pick a key column \eqn{j}; for every value \eqn{v} that
#' occurs among the violators, draft \eqn{k-1} violators carrying \eqn{v} and
#' blank all their *other* cells. Every record with value \eqn{v} in column
#' \eqn{j} is then wildcard-compatible with those \eqn{k-1} hubs, every record
#' genuinely missing in \eqn{j} is compatible with all of them, no hub is
#' released all-`NA` (so the contributor-liveness clause never fires), and safe
#' records stay safe by the violator-only soundness lemma. The cost is
#' \deqn{(k-1)\,d_j\,(p-1),}
#' with \eqn{d_j} the number of distinct violator values in column \eqn{j} --
#' **independent of the number of records**. Drafting violators that already
#' carry genuine missing values makes it cheaper still.
#'
#' This is an *upper bound*, not an optimiser. Where the exact optimum is
#' known it is markedly worse (e.g. 6 against 3 on `testdata` with
#' four keys at k = 3); its value is that it is cheap, closed-form, and
#' independent of \eqn{n}, which is what makes the deployed sweep's
#' \eqn{\Theta(|V|)} spending look the way it does
#' (`benchmarks/e-hub-construction.txt`).
#'
#' @param x A data.frame of microdata.
#' @param keyVars Key variables, as column names or indices.
#' @param k Required minimum frequency.
#' @param alpha Wildcard weight of the deployed counter, as in
#'   [ls_check()]. The models here are exact for `alpha = 1` (the deployed
#'   default) only; other values are refused rather than silently ignored,
#'   because intermediate `alpha` gives fractional count coefficients.
#' @param column Optional: restrict the construction to this key column (name
#'   or index into `keyVars`). By default every column is tried and the
#'   cheapest feasible one is returned.
#' @return A list with `xAnon` (the released file, `NULL` when no column
#'   yields a feasible construction), `nsupp`, `objective`, `column` (the key
#'   column used), `hubs` (row indices drafted), `feasible`, and `bound`
#'   (the generic \eqn{(k-1) d_j (p-1)} value for the chosen column).
#' @seealso [ls_greedy2()] for the heuristic, [ls_optimal()] for the exact
#'   reference, [ls_lns()] for the matheuristic.
#' @export
ls_hub <- function(x, keyVars, k = 2, column = NULL, alpha = 1) {
  stopifnot(is.data.frame(x), all(k >= 1L))
  if (!isTRUE(all.equal(alpha, 1))) {
    stop("the solvers are exact for alpha = 1 only; use ls_check(alpha = ) ",
         "to evaluate a released file at another alpha.")
  }
  kv_all <- if (length(k) == 1L) rep(k, nrow(x)) else k
  key <- as.data.frame(x[, keyVars, drop = FALSE])
  key[] <- lapply(key, as.character)
  p <- ncol(key)
  nms <- names(key)

  V <- ls_check(x, keyVars = keyVars, k = k, semantics = "wildcard")$violators
  if (length(V) == 0L) {
    return(list(xAnon = x, nsupp = 0L, objective = 0, column = NA_character_,
                hubs = integer(0), feasible = TRUE, bound = 0))
  }

  cols <- if (is.null(column)) seq_len(p) else {
    if (is.character(column)) match(column, nms) else as.integer(column)
  }

  best <- NULL
  for (j in cols) {
    vals <- unique(stats::na.omit(key[[j]][V]))
    hubs <- integer(0)
    for (v in vals) {
      cand <- V[which(key[[j]][V] == v)]
      # cheapest hubs first: a violator with genuine NAs costs less to strip
      cost <- rowSums(!is.na(key[cand, -j, drop = FALSE]))
      # every record carrying v must reach its own threshold, so the value
      # needs as many hubs as the largest threshold among them demands
      need_v <- max(kv_all[which(key[[j]] == v)]) - 1L
      hubs <- c(hubs, cand[order(cost, cand)][seq_len(min(need_v, length(cand)))])
    }
    if (!length(hubs)) next

    xa <- x
    n <- 0L
    for (i in hubs) {
      other <- keyVars[-j]
      n <- n + sum(!is.na(key[i, -j, drop = FALSE]))
      xa[i, other] <- NA
    }
    if (!ls_check(xa, keyVars = keyVars, k = k, semantics = "wildcard")$ok) next
    if (is.null(best) || n < best$nsupp) {
      best <- list(xAnon = xa, nsupp = as.integer(n), objective = as.numeric(n),
                   column = nms[j], hubs = sort(unique(hubs)), feasible = TRUE,
                   bound = (max(kv_all) - 1) * length(vals) * (p - 1))
    }
  }

  if (is.null(best)) {
    return(list(xAnon = NULL, nsupp = NA_integer_, objective = NA_real_,
                column = NA_character_, hubs = integer(0), feasible = FALSE,
                bound = NA_real_))
  }
  best
}
