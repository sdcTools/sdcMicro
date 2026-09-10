#' Exact minimum-cost local suppression
#'
#' Solves local suppression to optimality as a mixed-integer linear program,
#' rather than greedily as [kAnon()] does. This is the exact
#' reference the heuristic is measured against (experiment E1 of
#' `docs/04-scaling-and-heuristics.md`).
#'
#' @section Criterion:
#' The target is *k*-anonymity under the wildcard semantics **as implemented
#' by** [freqCalc()], which [ls_check()] delegates to. Measured on
#' probes (2026-09-01, see `docs/03-formulations.md`), that criterion is:
#' record `l` counts towards record `i`'s frequency iff (a) they agree on
#' every key where both are observed in the released data, and (b) **not**
#' (`l` is entirely missing while `i` is fully observed). Clause (b) makes the
#' contribution non-monotone -- fully suppressing a record silently withdraws
#' its credit from every fully-observed record -- and is modelled with
#' directed credit variables and liveness constraints below. A record's own
#' count always includes itself.
#'
#' @section Formulations:
#' Both formulations solve the same problem exactly and share the criterion
#' above; they differ in how the per-record decision is encoded.
#' \describe{
#'   \item{`"modelA"`}{One binary per cell. Compact, but the LP relaxation is
#'     weak (fractional cells half-satisfy the pairwise linking).}
#'   \item{`"modelB"`}{Per violator, one binary per suppression *pattern*
#'     (subset of its observed keys), tied to the cell layer by linking
#'     equalities -- a Dantzig--Wolfe convexification of the record
#'     subproblem, so its LP bound dominates model A's. The whole safe-class
#'     mass a pattern reaches is a precomputed constant (subset-sum over the
#'     class disagreement masks), which removes the class variables entirely.
#'     Mutual violator credits keep the model A pair machinery, so --
#'     unlike the naive "count a pattern against the unsuppressed data"
#'     variant -- two violators may repair each other.}
#' }
#'
#' @section Model:
#' Violator-only suppression (cells of records that already meet the target
#' are never suppressed -- exactly the restriction `kAnon()`
#' operates under). Constraints are generated only for the initial violators;
#' a soundness lemma (`docs/03` section 1) shows safe records can never
#' become violators, even under the non-monotone criterion. Safe records
#' enter only through their equivalence class (identical observed tuple)
#' with multiplicity, coefficients capped at the receiver's remaining need
#' -- the subset-of-observations reduction in aggregated form. The
#' violator-only optimum is a lower bound for violator-only methods, not the
#' unrestricted optimum (donor suppressions are experiment E2).
#'
#' @param x A data.frame of microdata.
#' @param keyVars Key variables, as column names or indices.
#' @param k Required minimum frequency.
#' @param importance Optional integer vector, one per key variable, in
#'   `sdcMicro` semantics: **1 is the most important** key and therefore the
#'   most expensive to suppress. Cell cost is `p + 1 - importance[j]`. When
#'   `NULL` every cell costs 1.
#' @param max_per_record Optional cap on the number of suppressed cells per
#'   record (a plain row-sum constraint, motivated by the hub optima of
#'   `benchmarks/e-grid-eusilcs.txt`: unit-cost optima may erase single
#'   records almost completely). With a cap the problem can be
#'   **infeasible** -- the always-feasible full-blank escape is excluded --
#'   and the solver then reports `status = "infeasible"`.
#' @param semantics Only `"wildcard"` (S1 as implemented by `freqCalc()`) is
#'   available so far.
#' @param formulation `"modelA"` (cell binaries) or `"modelB"` (pattern
#'   binaries per violator; tighter relaxation). See the Formulations section.
#' @param strataVars Optional stratum variables (names or indices). sdcMicro's
#'   semantics is strictly per stratum, so the problem decomposes exactly: each
#'   stratum is solved independently and the reported `objective`, `bound`,
#'   `nsupp`, `time` and `dims` are sums; `gap` is the worst stratum gap;
#'   `status` is `"optimal"` only if every stratum closed.
#' @param warm_start `"kAnon"` seeds the solver with `kAnon()`'s
#'   suppression pattern projected onto the violators' cells (an incumbent
#'   upper bound; ignored by the `scip` backend). `"none"` disables it.
#' @param solver,time_limit,gap Passed to [solve_milp()].
#' @param alpha Wildcard weight of the deployed counter, as in
#'   [ls_check()]. The models here are exact for `alpha = 1` (the deployed
#'   default) only; other values are refused rather than silently ignored,
#'   because intermediate `alpha` gives fractional count coefficients.
#' @return A list with `xAnon` (the suppressed data), `objective`, `bound`,
#'   `gap`, `status`, `time`, `solver`, `violators` (initial violators),
#'   `nsupp` (number of newly suppressed cells) and `dims` (model size:
#'   variables, constraints, safe classes).
#' @seealso [ls_check()], which verifies the result independently.
#' @export
ls_optimal <- function(x, keyVars, k = 2, alpha = 1, importance = NULL,
                       semantics = "wildcard",
                       formulation = c("modelA", "modelB"),
                       strataVars = NULL,
                       warm_start = c("none", "kAnon", "greedy2"),
                       solver = c("highs", "scip", "gurobi"),
                       time_limit = Inf, gap = 0, max_per_record = NULL) {
  semantics <- match.arg(semantics, "wildcard")
  formulation <- match.arg(formulation)
  warm_start <- match.arg(warm_start)
  solver <- match.arg(solver)
  stopifnot(is.data.frame(x))
  if (!isTRUE(all.equal(alpha, 1))) {
    stop("the solvers are exact for alpha = 1 only; use ls_check(alpha = ) ",
         "to evaluate a released file at another alpha.")
  }

  if (!is.null(strataVars)) {
    grp <- interaction(x[, strataVars, drop = FALSE], drop = TRUE)
    xAnon <- x
    tot <- list(objective = 0, bound = 0, gap = 0, time = 0, nsupp = 0L,
                vars = 0L, cons = 0L, classes = 0L)
    status <- "optimal"
    violators <- integer(0)
    for (g in levels(grp)) {
      idx <- which(grp == g)
      r <- ls_optimal(x[idx, , drop = FALSE], keyVars = keyVars,
                      k = if (length(k) == 1L) k else k[idx],
                      importance = importance, semantics = semantics,
                      formulation = formulation, warm_start = warm_start,
                      solver = solver, time_limit = time_limit, gap = gap,
                      max_per_record = max_per_record)
      xAnon[idx, keyVars] <- r$xAnon[, keyVars, drop = FALSE]
      tot$objective <- tot$objective + r$objective
      tot$bound <- tot$bound + r$bound
      tot$gap <- max(tot$gap, r$gap)
      tot$time <- tot$time + r$time
      tot$nsupp <- tot$nsupp + r$nsupp
      tot$vars <- tot$vars + r$dims[["vars"]]
      tot$cons <- tot$cons + r$dims[["cons"]]
      tot$classes <- tot$classes + max(r$dims[["classes"]], 0L, na.rm = TRUE)
      violators <- c(violators, idx[r$violators])
      if (!identical(r$status, "optimal")) status <- r$status
    }
    return(list(xAnon = xAnon, objective = tot$objective, bound = tot$bound,
                gap = tot$gap, status = status, time = tot$time,
                solver = solver, violators = sort(violators),
                nsupp = tot$nsupp,
                dims = c(vars = tot$vars, cons = tot$cons,
                         classes = tot$classes)))
  }

  mod <- ls_build_model(x, keyVars, k, importance, formulation,
                        max_per_record = max_per_record)
  if (is.null(mod)) {
    return(list(xAnon = x, objective = 0, bound = 0, gap = 0,
                status = "optimal", time = 0, solver = solver,
                violators = integer(0), nsupp = 0L,
                dims = c(vars = 0L, cons = 0L, classes = NA_integer_)))
  }

  ## ---- warm start ----------------------------------------------------------
  ws <- NULL
  start <- NULL
  if (warm_start != "none") {
    ws <- ls_warm_start(x, keyVars, k, importance, mod, method = warm_start)
    if (!is.null(ws)) start <- ws$start
  }

  sol <- solve_milp(obj = mod$obj, A = mod$A, sense = mod$sense,
                    rhs = mod$rhs, types = mod$types,
                    lower = mod$lower, upper = mod$upper, start = start,
                    time_limit = time_limit, gap = gap, solver = solver)

  ## ---- never worse than the warm start -------------------------------------
  ## Solvers may reject a partial MIP start (observed with highs on testdata
  ## 6 keys) or time out without an incumbent; the warm-start solution is a
  ## feasible fallback, and its cost caps the reported objective.
  if (!is.null(ws)) {
    have <- sol$status %in% c("optimal", "time_limit") &&
      !anyNA(sol$solution[seq_len(mod$n_x)]) && is.finite(sol$objective)
    if (!have || sol$objective > ws$cost + 1e-9) {
      bound <- sol$bound
      gp <- if (is.finite(bound) && ws$cost > 0) {
        max(0, (ws$cost - bound) / ws$cost)
      } else sol$gap
      return(list(xAnon = ws$xAnon, objective = ws$cost, bound = bound,
                  gap = gp, status = sol$status, time = sol$time,
                  solver = sol$solver, violators = mod$V, nsupp = ws$nsupp,
                  dims = c(vars = mod$nvar, cons = mod$ncons,
                           classes = mod$m)))
    }
  }

  xAnon <- ls_apply_solution(x, keyVars, mod, sol)
  list(xAnon = xAnon$x, objective = sol$objective, bound = sol$bound,
       gap = sol$gap, status = sol$status, time = sol$time,
       solver = sol$solver, violators = mod$V, nsupp = xAnon$nsupp,
       dims = c(vars = mod$nvar, cons = mod$ncons, classes = mod$m))
}

#  Internal: build the MILP for one stratum. Returns NULL when there is
#  nothing to do (no violators). All row/column bookkeeping the callers need
#  (LNS fixes cells, the warm start seeds them) travels in the result.
ls_build_model <- function(x, keyVars, k, importance, formulation,
                           max_per_record = NULL) {
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
  if (nV == 0L) return(NULL)

  km  <- as.matrix(key)
  vm  <- km[V, , drop = FALSE]          # violator tuples
  obs <- !is.na(vm)                     # originally observed cells
  o_i <- rowSums(obs)
  stopifnot(all(o_i >= 1L))             # an all-NA record has fk = n >= k
  vc  <- o_i == p                       # originally complete receivers

  ## ---- safe classes (the aggregation) ------------------------------------
  idx_safe <- setdiff(seq_len(n), V)
  if (length(idx_safe) > 0L) {
    dt <- data.table::as.data.table(km[idx_safe, , drop = FALSE])
    data.table::setnames(dt, paste0("K", seq_len(p)))
    dt[, ".sfrow" := idx_safe]
    agg <- dt[, list(sz = .N, ridx = .sfrow[1L]), by = c(paste0("K", seq_len(p)))]
    sm  <- km[agg$ridx, , drop = FALSE]
    csz <- agg$sz
  } else {
    sm  <- km[integer(0), , drop = FALSE]
    csz <- integer(0)
  }
  m <- length(csz)
  cls_allNA <- if (m) rowSums(!is.na(sm)) == 0L else logical(0)

  ## ---- disagreement structure (vectorised) -------------------------------
  differ_mat <- function(a, b) {
    outer(a, b, function(u, v) !is.na(u) & !is.na(v) & u != v)
  }
  Dlist <- vector("list", p)               # violator x class, per key
  Wlist <- vector("list", p)               # violator x violator, per key
  for (j in seq_len(p)) {
    Dlist[[j]] <- if (m) differ_mat(vm[, j], sm[, j]) else
      matrix(FALSE, nV, 0)
    Wlist[[j]] <- differ_mat(vm[, j], vm[, j])
  }
  Dn <- Reduce(`+`, Dlist)
  Wn <- Reduce(`+`, Wlist)

  ## ---- base counts (model A bookkeeping; model B folds classes into
  ## pattern masses instead) -------------------------------------------------
  ok_cls  <- if (m) (Dn == 0L) else matrix(FALSE, nV, 0)
  baseA   <- rep(1, nV) +
    (if (m) as.numeric(ok_cls %*% ifelse(cls_allNA, 0, csz)) else rep(0, nV)) +
    (if (m) as.numeric(ok_cls %*% ifelse(cls_allNA, csz, 0)) * !vc else rep(0, nV))
  allNA_mass <- if (m) sum(csz[cls_allNA]) else 0

  fk0 <- baseA + (rowSums(Wn == 0L) - 1)
  stopifnot(all(fk0 == chk$fk[V]))         # the internal referee

  ## ---- shared pair structure ----------------------------------------------
  n_x  <- nV * p
  xid  <- function(a, j) (a - 1L) * p + j

  pr   <- which(upper.tri(Wn) & Wn > 0L, arr.ind = TRUE)
  n_c  <- nrow(pr)
  cid_of <- matrix(0L, nV, nV)
  if (n_c) {
    cid_of[pr] <- seq_len(n_c)
    cid_of[pr[, c(2, 1), drop = FALSE]] <- seq_len(n_c)
  }
  Vc   <- which(vc)
  zp   <- if (length(Vc)) {
    zz <- cbind(l = rep(seq_len(nV), times = length(Vc)),
                i = rep(Vc, each = nV))
    zz[zz[, 1] != zz[, 2], , drop = FALSE]
  } else matrix(integer(0), 0, 2, dimnames = list(NULL, c("l", "i")))
  n_z  <- nrow(zp)

  modelB <- formulation == "modelB"

  ## ---- model-B pattern blocks ---------------------------------------------
  if (modelB) {
    pat <- vector("list", nV)   # per violator: masks, cost, capped mass
    for (a in seq_len(nV)) {
      keys  <- which(obs[a, ])
      o     <- length(keys)
      masks <- 0:(2^o - 1L)
      bitc  <- vapply(seq_len(o), function(b) bitwAnd(masks, 2L^(b - 1L)) > 0L,
                      logical(length(masks)))
      bitc  <- matrix(bitc, nrow = length(masks))
      cost  <- as.numeric(bitc %*% cost_j[keys])
      mass  <- numeric(length(masks))
      if (m) {
        keep <- which(!cls_allNA)
        if (length(keep)) {
          dmask <- integer(length(keep))
          for (b in seq_len(o)) {
            hasb <- Dlist[[keys[b]]][a, keep]
            dmask[hasb] <- dmask[hasb] + 2L^(b - 1L)
          }
          tab <- tapply(csz[keep], dmask, sum)
          mass[as.integer(names(tab)) + 1L] <- as.numeric(tab)
          for (b in seq_len(o)) {            # subset-sum (SOS) transform
            sel <- which(bitc[, b])
            mass[sel] <- mass[sel] + mass[sel - 2L^(b - 1L)]
          }
        }
        if (allNA_mass > 0) {
          if (vc[a]) mass[masks != 0L] <- mass[masks != 0L] + allNA_mass
          else       mass <- mass + allNA_mass
        }
      }
      pat[[a]] <- list(keys = keys, masks = masks, cost = cost, mass = mass,
                       full = 2^o - 1L)
    }
    npat  <- vapply(pat, function(z) length(z$masks), integer(1))
    loff  <- cumsum(c(0L, npat))[seq_len(nV)]
    n_l   <- sum(npat)
    lid0  <- function(a) loff[a] + 1L            # lambda for empty pattern
    lidF  <- function(a) loff[a] + npat[a]       # lambda for full pattern
  } else {
    n_l <- 0L
  }

  pc <- if (!modelB && m) {
    which(Dn > 0L & !matrix(cls_allNA, nV, m, byrow = TRUE), arr.ind = TRUE)
  } else matrix(integer(0), 0, 2)
  n_y <- nrow(pc)

  need_aux <- !modelB && length(Vc) > 0L && nV > 1L
  n_a <- if (need_aux) nV else 0L
  n_q <- if (!modelB) length(Vc) else 0L

  off_c <- n_x
  off_z <- off_c + n_c
  off_y <- off_z + n_z
  off_a <- off_y + n_y
  off_q <- off_a + n_a
  off_l <- off_q + n_q
  nvar  <- off_l + n_l
  qid   <- integer(nV); if (n_q) qid[Vc] <- off_q + seq_len(n_q)
  aid   <- if (n_a) off_a + seq_len(nV) else integer(0)

  need <- kv_all[V] - (if (modelB) rep(1, nV) else baseA)

  ## ---- constraint assembly -------------------------------------------------
  Ri <- list(); Ci <- list(); Xv <- list(); Sn <- list(); Rh <- list()
  nrow_now <- 0L
  push <- function(ri, ci, xv, sense, rhs) {
    Ri[[length(Ri) + 1L]] <<- ri + nrow_now
    Ci[[length(Ci) + 1L]] <<- ci
    Xv[[length(Xv) + 1L]] <<- xv
    Sn[[length(Sn) + 1L]] <<- sense
    Rh[[length(Rh) + 1L]] <<- rhs
    nrow_now <<- nrow_now + length(rhs)
  }

  # (A) c_il <= x_aj + x_bj on every disagreeing key
  if (n_c) for (j in seq_len(p)) {
    sel <- which(Wlist[[j]][pr])
    if (!length(sel)) next
    a <- pr[sel, 1]; b <- pr[sel, 2]; nq <- length(sel)
    push(rep(seq_len(nq), 3L),
         c(off_c + sel, xid(a, j), xid(b, j)),
         c(rep(1, nq), rep(-1, 2 * nq)),
         rep("<=", nq), rep(0, nq))
  }

  # (B) z_{l->i} <= c_il where the pair disagrees somewhere
  if (n_z) {
    cz <- cid_of[zp]
    sel <- which(cz > 0L)
    if (length(sel)) {
      nq <- length(sel)
      push(rep(seq_len(nq), 2L),
           c(off_z + sel, off_c + cz[sel]),
           c(rep(1, nq), rep(-1, nq)),
           rep("<=", nq), rep(0, nq))
    }
  }

  # (C) liveness for fully observed receivers
  if (n_z && !modelB) {
    nq <- n_z
    push(rep(seq_len(nq), 3L),
         c(off_z + seq_len(nq), aid[zp[, 1]], qid[zp[, 2]]),
         c(rep(1, nq), rep(1, nq), rep(-1, nq)),
         rep("<=", nq), rep(1, nq))
  }
  if (n_z && modelB) {
    # z + lambda_{l,full} + lambda_{i,empty} <= 2
    nq <- n_z
    push(rep(seq_len(nq), 3L),
         c(off_z + seq_len(nq),
           off_l + vapply(zp[, 1], lidF, integer(1)),
           off_l + vapply(zp[, 2], lid0, integer(1))),
         c(rep(1, nq), rep(1, nq), rep(1, nq)),
         rep("<=", nq), rep(2, nq))
  }

  # (D)/(E) allNA / incomp forcing (model A only)
  if (n_a) {
    oc <- which(obs, arr.ind = TRUE)
    push(c(seq_len(nV), oc[, 1]),
         c(aid, xid(oc[, 1], oc[, 2])),
         c(rep(1, nV), rep(-1, nrow(oc))),
         rep(">=", nV), 1 - o_i)
  }
  if (n_q) {
    oc <- which(obs[Vc, , drop = FALSE], arr.ind = TRUE)
    push(c(seq_len(n_q), oc[, 1]),
         c(qid[Vc], xid(Vc[oc[, 1]], oc[, 2])),
         c(rep(1, n_q), rep(-1, nrow(oc))),
         rep("<=", n_q), rep(0, n_q))
  }

  # (F) y_{C->i} <= x_ij (model A only)
  if (n_y) for (j in seq_len(p)) {
    sel <- which(Dlist[[j]][pc])
    if (!length(sel)) next
    a <- pc[sel, 1]; nq <- length(sel)
    push(rep(seq_len(nq), 2L),
         c(off_y + sel, xid(a, j)),
         c(rep(1, nq), rep(-1, nq)),
         rep("<=", nq), rep(0, nq))
  }

  # (L)+(H) model-B linking and convexity
  if (modelB) {
    for (a in seq_len(nV)) {
      keys <- pat[[a]]$keys; masks <- pat[[a]]$masks
      for (b in seq_along(keys)) {
        inS <- which(bitwAnd(masks, 2L^(b - 1L)) > 0L)
        push(rep(1L, 1L + length(inS)),
             c(xid(a, keys[b]), off_l + loff[a] + inS),
             c(1, rep(-1, length(inS))),
             "==", 0)
      }
      push(rep(1L, npat[a]), off_l + loff[a] + seq_len(npat[a]),
           rep(1, npat[a]), "==", 1)
    }
  }

  # (G) counting constraint per violator
  cnt_ri <- integer(0); cnt_ci <- integer(0); cnt_xv <- numeric(0)
  cnt_rhs <- numeric(0)
  for (a in seq_len(nV)) {
    ci <- integer(0); xv <- numeric(0)
    rhs_a <- need[a]
    if (vc[a]) {
      sel <- which(zp[, 2] == a)
      ci <- c(ci, off_z + sel); xv <- c(xv, rep(1, length(sel)))
      if (!modelB && allNA_mass > 0) {
        ci <- c(ci, qid[a]); xv <- c(xv, min(allNA_mass, max(rhs_a, 1)))
      }
    } else {
      ids <- unique(cid_of[a, ][cid_of[a, ] > 0L])
      ci <- c(ci, off_c + ids); xv <- c(xv, rep(1, length(ids)))
      rhs_a <- rhs_a - (sum(Wn[a, ] == 0L) - 1)
    }
    if (modelB) {
      mass_cap <- pmin(pat[[a]]$mass, max(rhs_a, 1))
      selm <- which(mass_cap > 0)
      ci <- c(ci, off_l + loff[a] + selm)
      xv <- c(xv, mass_cap[selm])
    } else if (n_y) {
      sel <- which(pc[, 1] == a)
      if (length(sel)) {
        ci <- c(ci, off_y + sel)
        xv <- c(xv, pmin(csz[pc[sel, 2]], max(rhs_a, 1)))
      }
    }
    if (rhs_a <= 0) next
    if (!length(ci)) stop("Record ", V[a], " cannot reach k = ", kv_all[V[a]], ".")
    cnt_ri <- c(cnt_ri, rep(length(cnt_rhs) + 1L, length(ci)))
    cnt_ci <- c(cnt_ci, ci); cnt_xv <- c(cnt_xv, xv)
    cnt_rhs <- c(cnt_rhs, rhs_a)
  }
  if (length(cnt_rhs)) {
    push(cnt_ri, cnt_ci, cnt_xv, rep(">=", length(cnt_rhs)), cnt_rhs)
  }

  if (!is.null(max_per_record)) {
    stopifnot(length(max_per_record) == 1L, max_per_record >= 1L)
    push(rep(seq_len(nV), each = p), seq_len(n_x), rep(1, n_x),
         rep("<=", nV), rep(as.numeric(max_per_record), nV))
  }

  A <- Matrix::sparseMatrix(i = unlist(Ri), j = unlist(Ci), x = unlist(Xv),
                            dims = c(nrow_now, nvar))
  obj   <- c(rep(cost_j, times = nV), rep(0, nvar - n_x))
  types <- c(rep(if (modelB) "C" else "B", n_x),
             rep("C", off_l - n_x),
             rep("B", n_l))
  lower <- rep(0, nvar)
  upper <- rep(1, nvar)
  upper[seq_len(n_x)][!as.vector(t(obs))] <- 0

  list(obj = obj, A = A, sense = unlist(Sn), rhs = unlist(Rh), types = types,
       lower = lower, upper = upper, nvar = nvar, ncons = nrow_now,
       n_x = n_x, p = p, V = V, nV = nV, obs = obs, m = m, km = km,
       modelB = modelB,
       pat = if (modelB) pat else NULL,
       npat = if (modelB) npat else NULL,
       loff = if (modelB) loff else NULL,
       off_l = off_l)
}

#  Internal: run a heuristic (kAnon or greedy2) and project its suppression
#  pattern onto the model's variables. Returns the start vector plus the
#  heuristic's own solution and cost, so the caller can fall back to it.
ls_warm_start <- function(x, keyVars, k, importance, mod,
                          method = c("kAnon", "greedy2")) {
  method <- match.arg(method)
  if (method == "kAnon") {
    # kAnon() coerces keys with as.numeric() and silently suppresses nothing
    # on non-numeric character labels (same trap as in fk_wildcard). Recode
    # injectively first; only the suppression POSITIONS are read back, and
    # those are invariant under recoding.
    xr <- as.data.frame(x)
    for (v in keyVars) xr[[v]] <- as.integer(factor(xr[[v]]))
    h <- try(suppressWarnings(
      kAnon(xr, keyVars = keyVars, k = k,
                      importance = importance)), silent = TRUE)
    if (inherits(h, "try-error")) return(NULL)
    ha <- as.data.frame(h$xAnon)
    supp <- is.na(as.matrix(ha[, keyVars, drop = FALSE])) & !is.na(mod$km)
  } else {
    g <- try(ls_greedy2(x, keyVars = keyVars, k = k, importance = importance),
             silent = TRUE)
    if (inherits(g, "try-error")) return(NULL)
    ga <- as.data.frame(g$xAnon)
    supp <- is.na(as.matrix(ga[, keyVars, drop = FALSE])) & !is.na(mod$km)
  }

  start <- rep(NA_real_, mod$nvar)
  for (a in seq_len(mod$nV)) {
    srow <- supp[mod$V[a], ]
    start[(a - 1L) * mod$p + seq_len(mod$p)] <- as.numeric(srow)
    if (mod$modelB) {
      keys <- mod$pat[[a]]$keys
      mk <- sum(2L^(seq_along(keys) - 1L) * as.integer(srow[keys]))
      lam <- numeric(mod$npat[a]); lam[mk + 1L] <- 1
      start[mod$off_l + mod$loff[a] + seq_len(mod$npat[a])] <- lam
    }
  }

  cost_j <- mod$obj[seq_len(mod$p)]
  ij <- which(supp, arr.ind = TRUE)
  xAnon <- x
  for (r in seq_len(nrow(ij))) xAnon[ij[r, 1L], keyVars[ij[r, 2L]]] <- NA
  list(start = start, xAnon = xAnon,
       cost = sum(cost_j[ij[, 2L]]), nsupp = nrow(ij))
}

#  Internal: turn a solver solution into the suppressed data.frame.
ls_apply_solution <- function(x, keyVars, mod, sol) {
  xAnon <- x
  nsupp <- 0L
  if (sol$status %in% c("optimal", "time_limit") &&
      !anyNA(sol$solution[seq_len(mod$n_x)])) {
    xv <- sol$solution[seq_len(mod$n_x)]
    hit <- which(matrix(xv > 0.5, mod$nV, mod$p, byrow = TRUE) & mod$obs,
                 arr.ind = TRUE)
    if (nrow(hit)) {
      for (r in seq_len(nrow(hit))) {
        xAnon[mod$V[hit[r, 1]], keyVars[hit[r, 2]]] <- NA
      }
      nsupp <- nrow(hit)
    }
  }
  list(x = xAnon, nsupp = nsupp)
}
