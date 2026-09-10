#' Improved greedy local suppression: exact per-record steps, cheapest first
#'
#' The `greedy2` heuristic of `docs/04-scaling-and-heuristics.md` section 2.
#' Where `kAnon()` suppresses *the first key, in
#' least-important-first order, on which a violator differs from a nearest
#' neighbour*, `ls_greedy2()` computes for every unresolved violator its
#' **exact minimum-cost repair pattern** against the *current* data (all
#' `2^p` patterns, evaluated cheapest-first with early stop) and applies the
#' cheapest repair in the file; records repaired passively by other records'
#' suppressions are skipped for free. Fully deterministic (ties broken by
#' cost, then pattern size, then record order).
#'
#' The safe-class mass a pattern reaches is precomputed once per violator
#' (subset-sum over class disagreement masks -- safe records never change
#' under violator-only suppression, by the soundness lemma of `docs/03`),
#' so each candidate evaluation costs O(number of violators), not O(n).
#' Counting follows the deployed criterion S1° exactly, including the
#' contributor-liveness clause; the loop recounts the initial violators until
#' none violates, so a blanked contributor withdrawing credit is caught.
#'
#' @param x A data.frame of microdata.
#' @param keyVars Key variables, as column names or indices.
#' @param k Required minimum frequency.
#' @param alpha Wildcard weight of the deployed counter, as in
#'   [ls_check()]. The models here are exact for `alpha = 1` (the deployed
#'   default) only; other values are refused rather than silently ignored,
#'   because intermediate `alpha` gives fractional count coefficients.
#' @param importance As in [ls_optimal()]: 1 = most important = most
#'   expensive to suppress.
#' @return A list with `xAnon`, `objective` (weighted cost of the new
#'   suppressions), `nsupp`, `violators` (initial violators) and `steps`
#'   (number of applied repair patterns).
#' @param engine `"cpp"` (default via `"auto"`) runs the compiled core --
#'   measured ~2-3 orders of magnitude faster on large violator sets, where
#'   the R loop needed 916 s at 772 violators; `"R"` runs the pure-R
#'   reference implementation. Both are pinned to produce identical output.
#' @seealso [ls_optimal()] for the exact reference, [ls_lns()] for the
#'   matheuristic that refines an incumbent.
#' @export
ls_greedy2 <- function(x, keyVars, k = 2, alpha = 1, importance = NULL,
                       engine = c("auto", "cpp", "R")) {
  engine <- match.arg(engine)
  if (engine == "auto") engine <- "cpp"
  stopifnot(is.data.frame(x), all(k >= 1L))
  if (!isTRUE(all.equal(alpha, 1))) {
    stop("the solvers are exact for alpha = 1 only; use ls_check(alpha = ) ",
         "to evaluate a released file at another alpha.")
  }
  key <- as.data.frame(x[, keyVars, drop = FALSE])
  key[] <- lapply(key, as.character)
  n <- nrow(key)
  p <- ncol(key)
  if (n < max(k)) {
    stop("k = ", max(k), " exceeds the number of records (", n, ").")
  }
  kv_all <- if (length(k) == 1L) rep(k, n) else k

  cost_j <- if (is.null(importance)) rep(1, p) else {
    stopifnot(length(importance) == p)
    p + 1 - as.numeric(importance)
  }

  chk <- ls_check(x, keyVars = keyVars, k = k, semantics = "wildcard")
  V <- chk$violators
  nV <- length(V)
  if (nV == 0L) {
    return(list(xAnon = x, objective = 0, nsupp = 0L,
                violators = integer(0), steps = 0L))
  }

  km <- as.matrix(key)
  kvec <- as.numeric(kv_all[V])   # per-violator thresholds
  vm <- km[V, , drop = FALSE]                 # original violator tuples
  obs <- !is.na(vm)
  o_i <- rowSums(obs)
  genNA <- o_i < p

  ## ---- static safe-class masses per violator and pattern -------------------
  idx_safe <- setdiff(seq_len(n), V)
  if (length(idx_safe)) {
    dt <- data.table::as.data.table(km[idx_safe, , drop = FALSE])
    data.table::setnames(dt, paste0("K", seq_len(p)))
    dt[, ".sfrow" := idx_safe]
    agg <- dt[, list(sz = .N, ridx = .sfrow[1L]),
              by = c(paste0("K", seq_len(p)))]
    sm <- km[agg$ridx, , drop = FALSE]
    csz <- agg$sz
  } else {
    sm <- km[integer(0), , drop = FALSE]; csz <- integer(0)
  }
  m <- length(csz)
  cls_allNA <- if (m) rowSums(!is.na(sm)) == 0L else logical(0)
  allNA_mass <- if (m) sum(csz[cls_allNA]) else 0

  pat <- vector("list", nV)
  for (a in seq_len(nV)) {
    keys <- which(obs[a, ])
    o <- length(keys)
    masks <- 0:(2L^o - 1L)
    bitc <- matrix(vapply(seq_len(o), function(b)
      bitwAnd(masks, 2L^(b - 1L)) > 0L, logical(length(masks))),
      nrow = length(masks))
    cost <- as.numeric(bitc %*% cost_j[keys])
    mass <- numeric(length(masks))
    if (m) {
      keep <- which(!cls_allNA)
      if (length(keep)) {
        dmask <- integer(length(keep))
        for (b in seq_len(o)) {
          jj <- keys[b]
          hasb <- !is.na(vm[a, jj]) & !is.na(sm[keep, jj]) &
            vm[a, jj] != sm[keep, jj]
          dmask[hasb] <- dmask[hasb] + 2L^(b - 1L)
        }
        tab <- tapply(csz[keep], dmask, sum)
        mass[as.integer(names(tab)) + 1L] <- as.numeric(tab)
        for (b in seq_len(o)) {
          sel <- which(bitc[, b])
          mass[sel] <- mass[sel] + mass[sel - 2L^(b - 1L)]
        }
      }
      if (allNA_mass > 0) {
        if (genNA[a]) mass <- mass + allNA_mass
        else mass[masks != 0L] <- mass[masks != 0L] + allNA_mass
      }
    }
    ordc <- order(cost, masks)                # cheapest first, deterministic
    pat[[a]] <- list(keys = keys, masks = masks[ordc], cost = cost[ordc],
                     mass = mass[ordc], full = 2L^o - 1L)
  }

  ## ---- solve: compiled core or the pure-R reference loop -------------------
  if (engine == "cpp") {
    vmI <- matrix(NA_integer_, nV, p)
    for (j in seq_len(p)) {
      vmI[, j] <- as.integer(factor(km[, j]))[V]
    }
    core <- cpp_greedy2_core(vmI,
                             lapply(pat, `[[`, "keys"),
                             lapply(pat, `[[`, "masks"),
                             lapply(pat, `[[`, "cost"),
                             lapply(pat, `[[`, "mass"),
                             kvec)
    supp_mask <- core$supp_mask
    steps <- core$steps
  } else {

  ## ---- current released violator tuples ------------------------------------
  cur <- vm                                   # suppressions land here as NA
  supp_mask <- integer(nV)                    # applied pattern per violator

  # S1-degree count of violator a if released with pattern `mk`:
  # 1 (self) + static safe mass + current violator partners.
  count_with <- function(a, mk, mass_mk) {
    keys <- pat[[a]]$keys
    rel <- vm[a, ]
    rel[keys[bitwAnd(mk, 2L^(seq_along(keys) - 1L)) > 0L]] <- NA
    rel_incomp <- anyNA(rel)
    cnt <- 1 + mass_mk
    for (b in seq_len(nV)) {
      if (b == a) next
      other <- cur[b, ]
      comp <- TRUE
      for (j in seq_len(p)) {
        if (!is.na(rel[j]) && !is.na(other[j]) && rel[j] != other[j]) {
          comp <- FALSE; break
        }
      }
      if (!comp) next
      if (all(is.na(other)) && !rel_incomp) next   # liveness clause
      cnt <- cnt + 1
    }
    cnt
  }

  cheapest_repair <- function(a) {
    pa <- pat[[a]]
    already <- supp_mask[a]
    for (q in seq_along(pa$masks)) {
      mk <- bitwOr(pa$masks[q], already)      # never undo prior suppressions
      if (mk != pa$masks[q]) next             # only supersets of current
      if (count_with(a, mk, pa$mass[q]) >= kvec[a]) {
        return(list(mask = mk, cost = pa$cost[q],
                    size = sum(bitwAnd(mk, 2L^(seq_along(pa$keys) - 1L)) > 0L)))
      }
    }
    NULL
  }

  current_fk <- function(a) {
    q <- match(supp_mask[a], pat[[a]]$masks)
    count_with(a, supp_mask[a], pat[[a]]$mass[q])
  }

  steps <- 0L
  repeat {
    unresolved <- which(vapply(seq_len(nV), current_fk, numeric(1)) < kvec)
    if (!length(unresolved)) break
    best <- NULL
    for (a in unresolved) {
      r <- cheapest_repair(a)
      if (is.null(r)) next
      extra <- r$cost - pat[[a]]$cost[match(supp_mask[a], pat[[a]]$masks)]
      if (is.null(best) || extra < best$extra - 1e-9 ||
          (abs(extra - best$extra) < 1e-9 && r$size < best$size) ||
          (abs(extra - best$extra) < 1e-9 && r$size == best$size &&
           a < best$a)) {
        best <- list(a = a, mask = r$mask, extra = extra, size = r$size)
      }
    }
    if (is.null(best)) stop("greedy2: no repair found for the remaining violators.")
    a <- best$a
    supp_mask[a] <- best$mask
    keys <- pat[[a]]$keys
    hit <- keys[bitwAnd(best$mask, 2L^(seq_along(keys) - 1L)) > 0L]
    cur[a, hit] <- NA
    steps <- steps + 1L
  }

  }

  ## ---- apply ---------------------------------------------------------------
  xAnon <- x
  nsupp <- 0L
  objective <- 0
  for (a in seq_len(nV)) {
    keys <- pat[[a]]$keys
    hit <- keys[bitwAnd(supp_mask[a], 2L^(seq_along(keys) - 1L)) > 0L]
    for (j in hit) {
      xAnon[V[a], keyVars[j]] <- NA
      nsupp <- nsupp + 1L
      objective <- objective + cost_j[j]
    }
  }

  list(xAnon = xAnon, objective = objective, nsupp = nsupp,
       violators = V, steps = steps)
}
