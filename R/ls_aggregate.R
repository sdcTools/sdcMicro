#' Exact aggregated model over original tuples and suppression patterns
#'
#' The formulation written down in the proof of the fixed-key-count
#' proposition (`docs/08-theory.md`; manuscript Proposition 2.16) and, until
#' now, never built. Its exactness is proved as manuscript Proposition 3.3
#' (`docs/08-theory.md` Theorem 4d.1): every feasible solution of the model
#' maps to a suppression pattern of equal cost and back, so its optimum is the
#' optimum of the problem and not of a restriction of it. It differs from
#' [ls_optimal()] in two ways that matter.
#' It is exact for the **unrestricted** problem: safe records may be
#' suppressed too, so donor moves are inside the model rather than excluded by
#' the violator-only restriction --- which this model shows to be *lossy*: on
#' `testdata` with four keys and `k = 3` the optimum is 2 against the
#' restricted optimum of 3, and it blanks a cell of a non-violator. It carries no
#' pair block, so its size grows with the number of *distinct tuples* rather
#' than with \eqn{|V|^2} --- 47 tuples on `testdata` at four keys against 4,580
#' records.
#'
#' Records sharing an original tuple are interchangeable when costs depend on
#' the column rather than on the record --- the single hypothesis the exactness
#' proof needs --- so a solution is determined by the
#' integers \eqn{\zeta_{t,S}} counting how many records of tuple `t` are
#' released with suppression pattern `S`. Each released tuple carries a
#' multiplicity, its count is the multiplicity mass of the released tuples
#' compatible with it, and a released tuple that occurs at all must reach `k` ---
#' linearised with one binary per released tuple.
#'
#' @param x A data.frame of microdata.
#' @param keyVars Key variables, as names or indices.
#' @param k Required minimum frequency: a single threshold, or one per record
#'   as produced by [ls_target()] --- an individual-risk bound, for instance,
#'   gives thresholds that differ across the file once survey weights are in
#'   play. Records sharing an original tuple are interchangeable only if they
#'   also share a threshold, so the aggregation partitions by (tuple,
#'   threshold) and the occupancy row is replicated once per distinct threshold
#'   that occupies a released tuple. A constant vector reproduces the scalar
#'   model exactly.
#' @param semantics Which criterion to enforce, as in [ls_check()].
#'   `"wildcard"` is the deployed reading S1-deg: a released tuple counts every
#'   tuple it is *compatible* with, less the all-missing mass when it is
#'   complete (the contributor-liveness clause). `"identical"` is classical
#'   k-anonymity: only identical released tuples count, a missing value being a
#'   value of its own. The criterion enters the model in exactly one place, the
#'   count row, so the classical case is this model with compatibility replaced
#'   by equality and no liveness term --- which also makes it smaller, since the
#'   count block becomes diagonal.
#' @param importance Optional integer vector, one per key variable, `1` = most
#'   important; cell cost is `p + 1 - importance[j]` as in [ls_optimal()].
#'   Per-cell costs are outside this model: they break the interchangeability
#'   of records sharing a tuple, which is what the aggregation rests on.
#' @param alpha The weight `freqCalc()` gives a contributor that
#'   itself carries a missing value, `1` (the deployed default) being full
#'   wildcard matching. Probing the counter pins the rule exactly: a record's
#'   own tuple always counts in full, a *complete* contributor counts 1, any
#'   other compatible contributor counts `alpha`, an all-missing contributor
#'   counts 0 towards a complete receiver (the liveness clause) and `alpha`
#'   otherwise, and an all-missing record's own count is `n`. Every one of
#'   these is a constant of the *released tuple*, so the count row stays linear
#'   and the model is exact for every `alpha`, not only for 1. Ignored for
#'   `semantics = "identical"`, which has no wildcards to discount.
#' @param solver Passed to [solve_milp()].
#' @param time_limit,gap Solver budget, as in [ls_optimal()].
#' @param max_tuples Refuse to build beyond this many distinct released tuples;
#'   the count rows are dense in the compatibility relation, so the model grows
#'   quadratically in that number.
#' @return As [ls_optimal()]: `xAnon`, `objective`, `bound`, `gap`, `status`,
#'   `time`, `solver`, `nsupp`, plus `dims` reporting tuples, patterns and
#'   released tuples.
#' @seealso [ls_optimal()], which solves the violator-only restriction with a
#'   pair-based model and scales differently.
#' @export
ls_aggregate <- function(x, keyVars, k = 2, importance = NULL,
                         semantics = c("wildcard", "identical"), alpha = 1,
                         solver = c("highs", "scip", "gurobi"),
                         time_limit = Inf, gap = 0, max_tuples = 4000) {
  solver <- match.arg(solver)
  semantics <- match.arg(semantics)
  stopifnot(length(alpha) == 1L, alpha >= 0, alpha <= 1)
  if (semantics == "identical" && !isTRUE(all.equal(alpha, 1))) {
    stop("alpha applies to the wildcard reading only; classical k-anonymity ",
         "counts identical tuples and has no wildcard credit to discount.")
  }
  stopifnot(is.data.frame(x), all(k >= 1L))
  if (length(k) != 1L && length(k) != nrow(x)) {
    stop("'k' must be a single threshold or one per record (see ls_target()).")
  }
  key <- as.data.frame(x[, keyVars, drop = FALSE])
  key[] <- lapply(key, as.character)
  n <- nrow(key); p <- ncol(key)
  cost_j <- if (is.null(importance)) rep(1, p) else {
    stopifnot(length(importance) == p)
    p + 1 - as.numeric(importance)
  }
  enc <- function(m) apply(m, 1, function(r)
    paste(ifelse(is.na(r), "\1", r), collapse = "\r"))

  ## ---- groups: distinct (original tuple, threshold) ------------------------
  k_rec <- if (length(k) == 1L) rep(as.numeric(k), n) else as.numeric(k)
  idc <- paste0(enc(as.matrix(key)), "\2", k_rec)
  f   <- factor(idc)
  nT  <- nlevels(f)
  rep_row <- match(levels(f), idc)
  T_key   <- as.matrix(key)[rep_row, , drop = FALSE]
  n_t     <- as.vector(table(f))
  k_t     <- k_rec[rep_row]              # the group's common threshold

  ## ---- (tuple, pattern) columns -------------------------------------------
  zt <- vector("list", nT); zc <- vector("list", nT); zr <- vector("list", nT)
  for (t in seq_len(nT)) {
    obs <- which(!is.na(T_key[t, ])); ot <- length(obs)
    masks <- seq_len(2^ot) - 1L
    rels <- matrix(rep(T_key[t, ], each = length(masks)), nrow = length(masks))
    cst <- numeric(length(masks))
    for (a in seq_along(masks)) {
      sel <- obs[bitwAnd(masks[a], 2L^(seq_len(ot) - 1L)) > 0]
      if (length(sel)) { rels[a, sel] <- NA; cst[a] <- sum(cost_j[sel]) }
    }
    zt[[t]] <- rep(t, length(masks)); zc[[t]] <- cst; zr[[t]] <- enc(rels)
  }
  zt <- unlist(zt); zc <- unlist(zc); zr <- unlist(zr)
  nZ <- length(zt)

  rf <- factor(zr)
  nR <- nlevels(rf)
  if (nR > max_tuples) {
    stop("aggregated model refused: ", nR, " released tuples exceeds ",
         "max_tuples = ", max_tuples, "; the count rows grow quadratically.")
  }
  R_key <- do.call(rbind, strsplit(levels(rf), "\r", fixed = TRUE))
  R_key[R_key == "\1"] <- NA
  z_rel <- as.integer(rf)

  ## ---- which released tuples count towards which --------------------------
  ## the ONLY place the criterion enters the model
  if (semantics == "wildcard") {
    comp <- matrix(TRUE, nR, nR)
    for (j in seq_len(p)) {
      a  <- R_key[, j]
      eq <- outer(a, a, "==")
      eq[is.na(eq)] <- TRUE               # missing on either side: no clash
      comp <- comp & eq
    }
  } else {
    comp <- diag(TRUE, nR)                # classical: only identical tuples
  }
  obs_R    <- rowSums(!is.na(R_key))
  complete <- (obs_R == p) & semantics == "wildcard"   # no liveness under S2
  allNA    <- which(obs_R == 0)

  ## credit of contributor v towards receiver u, exactly as freqCalc weights it
  ## (verified against it on 300 random files x 5 alphas, benchmarks/alpha-rule.R)
  W <- matrix(1, nR, nR)
  if (semantics == "wildcard") {
    for (u in seq_len(nR)) {
      w <- ifelse(obs_R < p, alpha, 1)               # carries a missing value
      if (length(allNA)) w[allNA] <- if (complete[u]) 0 else alpha
      w[u] <- 1                                      # own tuple: never discounted
      if (obs_R[u] == 0) w[] <- 1                    # all-missing receiver: count = n
      W[u, ] <- w
    }
  }

  ## the count block is dense in nR under the wildcard reading and diagonal
  ## under the classical one, which is why max_tuples only binds for the former

  ## ---- occupancy pairs: a released tuple, per threshold that can occupy it -
  pk <- k_t[zt]                                    # threshold of each column
  pair_id <- paste0(z_rel, "\2", pk)
  pf <- factor(pair_id); nY <- nlevels(pf)
  pair_row <- match(levels(pf), pair_id)
  P_rel <- z_rel[pair_row]; P_k <- pk[pair_row]
  z_pair <- as.integer(pf)

  ## ---- variables: zeta (nZ) | M (nR) | y (nY) ------------------------------
  iZ <- seq_len(nZ); iM <- nZ + seq_len(nR); iY <- nZ + nR + seq_len(nY)
  nvar <- nZ + nR + nY
  obj <- c(zc, rep(0, nR + nY))

  rows <- list(); cols <- list(); vals <- list(); sense <- character(0); rhs <- numeric(0)
  add <- function(cc, vv, se, rr) {
    r <- length(sense) + 1L
    rows[[length(rows) + 1L]] <<- rep(r, length(cc))
    cols[[length(cols) + 1L]] <<- cc; vals[[length(vals) + 1L]] <<- vv
    sense <<- c(sense, se); rhs <<- c(rhs, rr)
  }
  for (t in seq_len(nT)) add(iZ[zt == t], rep(1, sum(zt == t)), "==", n_t[t])
  for (u in seq_len(nR)) {                       # multiplicity of each release
    z_u <- iZ[z_rel == u]
    add(c(iM[u], z_u), c(1, rep(-1, length(z_u))), "==", 0)
  }
  for (q in seq_len(nY)) {                       # one count row per (release, threshold)
    u <- P_rel[q]
    z_q <- iZ[z_pair == q]
    add(c(z_q, iY[q]), c(rep(1, length(z_q)), -n), "<=", 0)
    cv <- which(comp[u, ])
    add(c(iM[cv], iY[q]), c(W[u, cv], -P_k[q]), ">=", 0)
  }
  A <- Matrix::sparseMatrix(i = unlist(rows), j = unlist(cols), x = unlist(vals),
                            dims = c(length(sense), nvar))

  t0 <- proc.time()[["elapsed"]]
  sol <- solve_milp(obj = obj, A = A, sense = sense, rhs = rhs,
                    types = c(rep("I", nZ), rep("C", nR), rep("B", nY)),
                    lower = rep(0, nvar),
                    upper = c(rep(n, nZ), rep(n, nR), rep(1, nY)),
                    time_limit = time_limit, gap = gap, solver = solver)

  ## ---- write the solution back: records of a tuple are interchangeable -----
  xAnon <- x
  if (!anyNA(sol$solution[iZ])) {
    rows_of <- split(seq_len(n), f)
    for (t in seq_len(nT)) {
      idx <- rows_of[[levels(f)[t]]]
      zs  <- which(zt == t); cnt <- round(sol$solution[iZ[zs]])
      pos <- 1L
      for (a in seq_along(zs)) {
        if (cnt[a] <= 0) next
        take <- idx[pos:(pos + cnt[a] - 1L)]; pos <- pos + cnt[a]
        rel  <- R_key[z_rel[zs[a]], ]
        for (j in which(is.na(rel) & !is.na(T_key[t, ])))
          xAnon[take, keyVars[j]] <- NA
      }
    }
  }
  list(xAnon = xAnon, objective = sol$objective, bound = sol$bound,
       gap = sol$gap, status = sol$status,
       time = proc.time()[["elapsed"]] - t0, solver = solver,
       nsupp = sum(is.na(xAnon[, keyVars])) - sum(is.na(key)),
       semantics = semantics, alpha = alpha,
       dims = c(groups = nT, patterns = nZ, released = nR, occupancy = nY,
                vars = nvar, cons = length(sense)))
}
