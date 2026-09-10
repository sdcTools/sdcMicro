# Verification tests for the theory note docs/08-theory.md. Each test is the
# empirical half of a stated lemma/theorem (docs/07 work packages T1/T3, item
# A2: "the tests are the empirical half of the proofs"). If one of these ever
# fails, the corresponding proof in docs/08 is wrong -- fix the note, not the
# test.

# --- Theorem: separation family, S1*/S2* -> 0 (docs/08 s. 4) -----------------
# Hub family H_m: n = m+1 records over p = 2 keys, tuples (1, j), j = 1..m+1.
# All records are singletons under both semantics. One suppression (blank K2
# of one record) makes that record a wildcard hub compatible with everyone:
# S1* = 1. Under S2 every record must be touched: S2* = m+1.

hub_family <- function(m) {
  data.frame(K1 = rep("1", m + 1L), K2 = as.character(seq_len(m + 1L)))
}

test_that("hub family: S1 optimum is 1 for every m (one wildcard hub)", {
  for (m in 2:6) {
    x <- hub_family(m)
    res <- ls_optimal(x, c("K1", "K2"), k = 2)
    expect_identical(res$status, "optimal")
    expect_equal(res$objective, 1)
    expect_equal(res$nsupp, 1L)
    expect_true(ls_check(res$xAnon, c("K1", "K2"), k = 2)$ok)
  }
})

test_that("hub family: S2 optimum is m+1 (every record must be touched)", {
  for (m in 2:4) {
    x <- hub_family(m)
    expect_equal(
      brute_force_optimum(x, c("K1", "K2"), k = 2, semantics = "identical"),
      m + 1
    )
  }
})

test_that("hub family: the wildcard/identical ratio 1/(m+1) is unbounded below", {
  ratios <- vapply(2:4, function(m) {
    x <- hub_family(m)
    s1 <- brute_force_optimum(x, c("K1", "K2"), k = 2)
    s2 <- brute_force_optimum(x, c("K1", "K2"), k = 2, semantics = "identical")
    s1 / s2
  }, numeric(1))
  expect_equal(ratios, 1 / (3:5))
  expect_true(all(diff(ratios) < 0))
})

# --- Theorem: S1(deployed)* <= S2* on arbitrary instances (docs/08 s. 4) -----

test_that("wildcard optimum never exceeds identical-tuple optimum (random)", {
  set.seed(4711)
  for (rep in 1:10) {
    x <- data.frame(
      K1 = sample(c("1", "2", "3", NA), 5, replace = TRUE),
      K2 = sample(c("1", "2", NA), 5, replace = TRUE)
    )
    s1 <- brute_force_optimum(x, c("K1", "K2"), k = 2)
    s2 <- brute_force_optimum(x, c("K1", "K2"), k = 2, semantics = "identical")
    expect_true(s1 <= s2)
  }
})

# --- Proposition: S1-degree counts are non-monotone under S1-deployed --------
# Witness (docs/08 s. 1): {(2,2), (2,NA)} is 2-anonymous; blanking the last
# observed cell of the *safe* record 2 (a donor move) withdraws its credit
# from the complete record 1, which becomes a violator. Consequence: donor
# suppressions can CREATE violators under the deployed criterion; the
# violator-only soundness lemma is sharp.

test_that("a donor suppression can create a violator (liveness withdrawal)", {
  x <- data.frame(K1 = c("2", "2"), K2 = c("2", NA))
  before <- ls_check(x, c("K1", "K2"), k = 2)
  expect_true(before$ok)
  expect_equal(before$fk, c(2, 2))

  y <- x
  y[2, "K1"] <- NA                      # record 2 is now all-NA
  after <- ls_check(y, c("K1", "K2"), k = 2)
  expect_false(after$ok)
  expect_equal(after$fk, c(1, 2))       # record 1 lost its only contributor
  expect_identical(after$violators, 1L)
})

# --- Lemma: violator-only soundness, strong form (docs/08 s. 3) --------------
# Under violator-only suppression every initially safe record keeps a count
# >= its original count. Tested on random instances for both the exact solver
# and greedy2 (both suppress violator cells only).

test_that("violator-only suppression never decreases a safe record's count", {
  set.seed(2026)
  for (rep in 1:10) {
    n <- 25L
    x <- data.frame(
      K1 = sample(c("1", "2", "3", NA), n, replace = TRUE,
                  prob = c(0.4, 0.3, 0.2, 0.1)),
      K2 = sample(c("1", "2", NA), n, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
      K3 = sample(c("1", "2"), n, replace = TRUE)
    )
    kv <- c("K1", "K2", "K3")
    for (k in 2:3) {
      before <- ls_check(x, kv, k = k)
      safe <- setdiff(seq_len(n), before$violators)
      if (length(before$violators) == 0L) next

      opt <- ls_optimal(x, kv, k = k)
      after_o <- ls_check(opt$xAnon, kv, k = k)
      expect_true(after_o$ok)
      expect_true(all(after_o$fk[safe] >= before$fk[safe]))

      gr <- ls_greedy2(x, kv, k = k)
      after_g <- ls_check(gr$xAnon, kv, k = k)
      expect_true(after_g$ok)
      expect_true(all(after_g$fk[safe] >= before$fk[safe]))
    }
  }
})

# --- Correction: the docs/04 distance-truncation bound was false -------------
# Claimed there: "a violator at Hamming distance d from its (k-1)-th nearest
# record needs >= d suppressions". Witness: {(1,1), (2,2)}, k = 2. Pairwise
# distance 2, yet each record suppresses ONE cell -- the difference set is
# covered jointly (|S_i| + |S_l| >= d_il is the correct inequality). The
# all-NA route (blank one record fully) is infeasible here: liveness would
# withdraw its credit from the remaining complete record.

test_that("shared coverage: two crossed suppressions beat the distance bound", {
  x <- data.frame(K1 = c("1", "2"), K2 = c("1", "2"))
  res <- ls_optimal(x, c("K1", "K2"), k = 2)
  expect_identical(res$status, "optimal")
  expect_equal(res$objective, 2)        # not 4, and 1 is infeasible
  na_per_row <- rowSums(is.na(res$xAnon))
  expect_equal(unname(na_per_row), c(1, 1))
  expect_false(which(is.na(res$xAnon[1, ])) == which(is.na(res$xAnon[2, ])))
  expect_true(ls_check(res$xAnon, c("K1", "K2"), k = 2)$ok)
  expect_true(brute_force_optimum(x, c("K1", "K2"), k = 2) == 2)
})

# --- Theorem: weighted S1-MinLS is NP-hard already at k = 2 (docs/08 s. 4b) --
# Set-cover gadget. Columns: one activation column `a` plus one column per
# set. Elements are complete records (a = "e"; s_T = "1" if member, else a
# value unique to the element). Each set T gets a hub observing only
# {a, s_T} with a = "offT", s_T = "1". Initially every record is a singleton
# violator. Blanking a hub's a-cell (cost 1) makes it compatible with
# exactly the elements of its set (non-members still clash on s_T) and with
# every other hub; all other cells cost M. Hence the optimum equals the
# minimum set cover, and every suppression is on a violator cell.

sc_gadget <- function(membership) {
  m <- length(membership)
  n <- max(unlist(membership))
  el <- data.frame(a = rep("e", n), stringsAsFactors = FALSE)
  for (T in seq_len(m)) {
    el[[paste0("s", T)]] <- ifelse(seq_len(n) %in% membership[[T]],
                                   "1", paste0("q", seq_len(n)))
  }
  hub <- data.frame(a = paste0("off", seq_len(m)), stringsAsFactors = FALSE)
  for (T in seq_len(m)) {
    hub[[paste0("s", T)]] <- ifelse(seq_len(m) == T, "1", NA)
  }
  rbind(el, hub)
}

sc_costs <- function(n, m, M = 1000) {
  cm <- matrix(M, n + m, m + 1L)
  cm[n + seq_len(m), 1L] <- 1
  cm
}

test_that("set-cover gadget: every record starts as a singleton violator", {
  for (mem in list(list(1L, c(1L, 2L)), list(1L, 2L))) {
    x <- sc_gadget(mem)
    chk <- ls_check(x, names(x), k = 2)
    expect_equal(chk$fk, rep(1, nrow(x)))
    expect_identical(chk$violators, seq_len(nrow(x)))
  }
})

test_that("set-cover gadget: activating one covering hub is feasible", {
  # U = {1,2}, T1 = {1}, T2 = {1,2}: activating T2 covers everything.
  x <- sc_gadget(list(1L, c(1L, 2L)))
  y <- x
  y[4, "a"] <- NA                        # hub of T2
  chk <- ls_check(y, names(x), k = 2)
  expect_true(chk$ok)
  expect_equal(chk$fk, c(2, 2, 2, 4))    # e1, e2, unactivated h1, hub
})

test_that("set-cover gadget: optimum equals the minimum cover (brute force)", {
  # Minimum cover 1: T2 = {1,2} covers U alone.
  xA <- sc_gadget(list(1L, c(1L, 2L)))
  expect_equal(
    brute_force_optimum(xA, names(xA), k = 2, cost_j = sc_costs(2, 2)), 1)
  # Minimum cover 2: disjoint singleton sets, both needed.
  xB <- sc_gadget(list(1L, 2L))
  expect_equal(
    brute_force_optimum(xB, names(xB), k = 2, cost_j = sc_costs(2, 2)), 2)
})

test_that("set-cover gadget, k = 3: twins + beacon keep only elements tight", {
  # k >= 3 variant (docs/08 s. 4b). Naive twinning fails: for the activated
  # set the activation credit and the twin credit are the SAME record (this
  # test's first version proved that). Construction instead: k-1 identical
  # copies per element (f = k-1, violators), k-1 copies per hub plus a
  # beacon group of k records observing only a fresh column b -- the beacon
  # is compatible with every hub (no common observed column) but clashes
  # with every element on b, so hubs start safe at f = k and elements still
  # need exactly one activated covering hub.
  el1 <- data.frame(a = "e", s1 = "1", s2 = "1", b = "eb")
  el2 <- data.frame(a = "e", s1 = "q2", s2 = "1", b = "eb")
  h1  <- data.frame(a = "off1", s1 = "1", s2 = NA, b = NA)
  h2  <- data.frame(a = "off2", s1 = NA, s2 = "1", b = NA)
  bea <- data.frame(a = NA, s1 = NA, s2 = NA, b = "z")
  x <- rbind(el1, el1, el2, el2, h1, h1, h2, h2, bea, bea, bea)

  chk <- ls_check(x, names(x), k = 3)
  expect_equal(chk$fk, c(2, 2, 2, 2, 5, 5, 5, 5, 7, 7, 7))
  expect_identical(chk$violators, 1:4)   # elements only

  y <- x
  y[7, "a"] <- NA                        # activate one copy of hub T2
  after <- ls_check(y, names(y), k = 3)
  expect_true(after$ok)
  expect_equal(after$fk[1:4], rep(3, 4)) # elements exactly at k

  y1 <- x
  y1[5, "a"] <- NA                       # activating T1 covers element 1 only
  expect_identical(ls_check(y1, names(y1), k = 3)$violators, 3:4)
})

# --- Theorem: hardness survives per-COLUMN costs (docs/08 s. 4b, Thm 4b.2) ---
# sdcMicro's `importance` model prices whole columns. Per-set activation
# columns a_T restore the reduction: hub h_T observes only {a_T, s_T}, so
# hubs of different sets share no observed column and are compatible from
# the start (hubs are SAFE; activation is a donor move). An element can
# self-fix by blanking its own a_T cell at the same column cost as one
# activation -- and any self-fix can be swapped for activating a containing
# set, which covers at least as much. Hence optimum == minimum cover with
# only two column costs (a-columns 1, s-columns H).

pc_gadget <- function(membership) {
  m <- length(membership)
  n <- max(unlist(membership))
  el <- data.frame(row.names = seq_len(n))
  for (T in seq_len(m)) el[[paste0("a", T)]] <- rep("e", n)
  for (T in seq_len(m)) {
    el[[paste0("s", T)]] <- ifelse(seq_len(n) %in% membership[[T]],
                                   "1", paste0("q", seq_len(n)))
  }
  hub <- data.frame(row.names = n + seq_len(m))
  for (T in seq_len(m)) {
    hub[[paste0("a", T)]] <- ifelse(seq_len(m) == T, paste0("off", T), NA)
  }
  for (T in seq_len(m)) {
    hub[[paste0("s", T)]] <- ifelse(seq_len(m) == T, "1", NA)
  }
  rbind(el, hub)
}

test_that("per-column gadget: hubs are pairwise compatible and safe", {
  x <- pc_gadget(list(1L, c(1L, 2L)))
  chk <- ls_check(x, names(x), k = 2)
  expect_equal(chk$fk, c(1, 1, 2, 2))
  expect_identical(chk$violators, 1:2)   # elements only; hubs are donors
})

test_that("per-column gadget: optimum equals the minimum cover", {
  costs <- function(m) rep(c(1, 50), each = m)   # a-columns 1, s-columns H
  xA <- pc_gadget(list(1L, c(1L, 2L)))           # min cover 1
  expect_equal(
    brute_force_optimum(xA, names(xA), k = 2, cost_j = costs(2),
                        cells = "all"), 1)
  xB <- pc_gadget(list(1L, 2L))                  # disjoint sets: cover 2
  expect_equal(
    brute_force_optimum(xB, names(xB), k = 2, cost_j = costs(2),
                        cells = "all"), 2)
})

test_that("per-column gadget, k = 3: element copies stay the only violators", {
  x <- pc_gadget(list(1L, c(1L, 2L)))
  x2 <- rbind(x[c(1, 1, 2, 2), ], x[c(3, 3, 4, 4), ])  # k-1 = 2 copies each
  chk <- ls_check(x2, names(x2), k = 3)
  expect_equal(chk$fk, c(2, 2, 2, 2, 4, 4, 4, 4))
  expect_identical(chk$violators, 1:4)
  y <- x2
  y[7, "a2"] <- NA                       # activate one copy of hub T2
  after <- ls_check(y, names(y), k = 3)
  expect_true(after$ok)
  expect_equal(after$fk[1:4], rep(3, 4))
})

# --- Theorem: under S1-deployed, even ALL-UNIT costs are hard (Thm 4b.3) ----
# Unit costs are sdcMicro's default (importance = NULL). Two ingredients:
# (i) the liveness clause kills the universal donor -- under clean S1 one
# fully blanked record credits every record and caps the optimum at
# min_i o_i, but under S1-deployed an all-NA hub credits no complete
# element; (ii) a dummy empty set T_0 (column s_0 carrying the per-element
# unique values, hub h_0) makes every element pair differ in >= 2 columns,
# so pair-fixes cost >= 2 and the swap argument survives piggybacking.

test_that("liveness kills the universal donor: all-NA hub credits no element", {
  x <- data.frame(a0 = c("e", "off0", NA), a1 = c("e", NA, NA),
                  s0 = c("q1", "1", NA), s1 = c("1", NA, NA))
  # rows: element e1 (complete), hub h0, hub h1 fully blanked (all-NA)
  chk <- ls_check(x, names(x), k = 2)
  expect_equal(chk$fk, c(1, 2, 3))       # e1 gets NO credit from all-NA h1
  expect_identical(chk$violators, 1L)
})

test_that("unit costs, S1-deployed: optimum equals the cover on the gadget", {
  # U = {1}, T1 = {1}, plus the dummy T0: tau = 1. Full brute force over
  # all 12 cells at unit cost: exactly one suppression suffices and is
  # needed -- activation (hub a1) or the equally priced self-fix swap.
  x <- data.frame(a0 = c("e", "off0", NA), a1 = c("e", NA, "off1"),
                  s0 = c("q1", "1", NA), s1 = c("1", NA, "1"))
  chk <- ls_check(x, names(x), k = 2)
  expect_equal(chk$fk, c(1, 2, 2))
  expect_equal(
    brute_force_optimum(x, names(x), k = 2, cells = "all"), 1)
  y <- x
  y[3, "a1"] <- NA                       # activation
  expect_true(ls_check(y, names(y), k = 2)$ok)
  y2 <- x
  y2[1, "a1"] <- NA                      # the self-fix swap partner
  expect_true(ls_check(y2, names(y2), k = 2)$ok)
})

test_that("piggyback: an element can buy a credit from a hub that misses it", {
  # Found by the 2026-09-04 review gate. With hub T1 activated, an element
  # u NOT in T1 blanks its OWN s1 cell (cost 1): u and the activated hub then
  # co-observe nothing, so they are compatible and u is credited although T1
  # does not cover u. The unit-cost reduction therefore targets set cover
  # WITH unit-cost singletons, not plain set cover -- see docs/08 Thm 4b.3.
  el  <- data.frame(a0 = c("e", "e"), a1 = c("e", "e"), a2 = c("e", "e"),
                    s0 = c("q1", "q2"), s1 = c("1", "q2"), s2 = c("q1", "1"),
                    stringsAsFactors = FALSE)
  hub <- data.frame(a0 = c("off0", NA, NA), a1 = c(NA, "off1", NA),
                    a2 = c(NA, NA, "off2"),
                    s0 = c("1", NA, NA), s1 = c(NA, "1", NA),
                    s2 = c(NA, NA, "1"), stringsAsFactors = FALSE)
  x <- rbind(el, hub); kv <- names(x)
  y <- x; y[4, "a1"] <- NA                      # activate hub T1
  expect_equal(ls_check(y, kv, k = 2)$fk[2], 1) # element 2 still uncovered
  z <- y; z[2, "s1"] <- NA                      # element 2 buys a credit
  expect_equal(ls_check(z, kv, k = 2)$fk[2], 2)
  expect_true(ls_check(z, kv, k = 2)$fk[2] > ls_check(y, kv, k = 2)$fk[2])
})

test_that("T0 padding: distinct elements always differ in >= 2 columns", {
  # U = {1,2}, T1 = {1}, T2 = {2}, T0 = empty set. The q-values in s0 give
  # every element pair a second difference beyond membership, so no unit
  # pair-fix at cost 1 exists and covering stays optimal (tau = 2).
  el <- data.frame(a0 = c("e", "e"), a1 = c("e", "e"), a2 = c("e", "e"),
                   s0 = c("q1", "q2"), s1 = c("1", "q2"), s2 = c("q1", "1"))
  hub <- data.frame(a0 = c("off0", NA, NA), a1 = c(NA, "off1", NA),
                    a2 = c(NA, NA, "off2"),
                    s0 = c("1", NA, NA), s1 = c(NA, "1", NA),
                    s2 = c(NA, NA, "1"))
  x <- rbind(el, hub)
  chk <- ls_check(x, names(x), k = 2)
  expect_equal(chk$fk, c(1, 1, 3, 3, 3))
  # element pair differs in s0 AND both membership columns
  d <- which(!is.na(x[1, ]) & !is.na(x[2, ]) & x[1, ] != x[2, ])
  expect_true(length(d) >= 2)
  # activating the two real hubs is feasible at cost 2 = tau
  y <- x
  y[4, "a1"] <- NA
  y[5, "a2"] <- NA
  expect_true(ls_check(y, names(y), k = 2)$ok)
  # no single unit suppression is feasible (tau = 2 is tight)
  feas1 <- FALSE
  for (i in seq_len(nrow(x))) for (j in seq_along(x)) {
    if (is.na(x[i, j])) next
    z <- x; z[i, j] <- NA
    if (ls_check(z, names(z), k = 2)$ok) feas1 <- TRUE
  }
  expect_false(feas1)
})

# --- Theorem: component decomposition is impossible unconditionally ----------
# (docs/08 s. 6, settled 2026-09-03.) Witness: g = 3 groups of two records
# (v_i,v_i,v_i), (w_i,w_i,w_i), all six values distinct, p = 3, k = 2, no
# missing values -- per-column value-disjoint, the best case for any
# decomposition. Groupwise the optimum is 3 per group (pair at distance 3).
# But ONE record blanked fully becomes an all-NA universal donor crediting
# every INCOMPLETE receiver, so full blank + one cell on each other record
# costs 3 + 5 = 8 < 9. Cross-group help wins; decomposition is refuted.

dec_witness <- function(g = 3L) {
  vals <- as.character(seq_len(2L * g))
  do.call(rbind, lapply(seq_len(g), function(i) {
    data.frame(K1 = vals[c(2 * i - 1, 2 * i)], K2 = vals[c(2 * i - 1, 2 * i)],
               K3 = vals[c(2 * i - 1, 2 * i)])
  }))
}

test_that("groupwise optimum of the decomposition witness is 3 per group", {
  x <- dec_witness(3L)
  kv <- names(x)
  for (i in 1:3) {
    grp <- x[(2 * i - 1):(2 * i), ]
    expect_equal(brute_force_optimum(grp, kv, k = 2), 3)
  }
})

test_that("the all-NA donor beats the groupwise optimum across groups", {
  x <- dec_witness(3L)
  kv <- names(x)
  # explicit donor solution: record 1 fully blanked, one cell elsewhere
  y <- x
  y[1, kv] <- NA
  for (r in 2:6) y[r, "K3"] <- NA
  chk <- ls_check(y, kv, k = 2)
  expect_true(chk$ok)                        # feasible at cost 8
  # the exact solver confirms the true optimum is below 9 = groupwise
  res <- ls_optimal(x, kv, k = 2)
  expect_identical(res$status, "optimal")
  expect_lt(res$objective, 9)
  expect_lte(res$objective, 8)
  expect_true(ls_check(res$xAnon, kv, k = 2)$ok)
})

# --- Theorem: conditional decomposition under the per-record cap -------------
# With no genuine NAs and max_per_record <= ceiling(p/2) - 1 every record
# keeps more than p/2 cells, any two records co-observe a column, and
# value-disjoint groups stay mutually incompatible whatever is suppressed:
# the problem separates exactly. Instance: distance-1 pairs per group with
# group-specific values in every column; cap 1 at p = 3.

test_that("under the cap, value-disjoint groups solve independently", {
  g <- 3L
  x <- do.call(rbind, lapply(seq_len(g), function(i) {
    data.frame(K1 = rep(paste0("v", i), 2), K2 = rep(paste0("w", i), 2),
               K3 = paste0(c("a", "b"), i))
  }))
  kv <- names(x)
  res <- ls_optimal(x, kv, k = 2, max_per_record = 1)
  expect_identical(res$status, "optimal")
  expect_equal(res$objective, g)             # = sum of per-group optima (1 each)
  expect_true(all(rowSums(is.na(res$xAnon)) <= 1))
  expect_true(ls_check(res$xAnon, kv, k = 2)$ok)
})

# --- Theorem: LP(B) >= LP(A) (docs/08 s. 4c, T2) -----------------------------
# The Dantzig-Wolfe dominance, verified on random instances: relax both
# models to pure LPs and compare. Strictness must occur on some instance
# (measured factor 1.8-3.0 on the testdata hard rows).

test_that("the model-B LP relaxation dominates the model-A LP relaxation", {
  set.seed(1234)
  strict <- 0L
  tested <- 0L
  for (rep in 1:8) {
    n <- sample(15:30, 1)
    p <- sample(2:4, 1)
    x <- as.data.frame(
      replicate(p, sample(c(as.character(1:3), NA), n, TRUE,
                          prob = c(0.4, 0.3, 0.2, 0.1)),
                simplify = FALSE), stringsAsFactors = FALSE)
    names(x) <- paste0("K", seq_len(p))
    for (k in 2:3) {
      lp <- lapply(c("modelA", "modelB"), function(f) {
        mod <- sdcMicro:::ls_build_model(x, names(x), k, NULL, f)
        if (is.null(mod)) return(0)
        solve_milp(mod$obj, mod$A, mod$sense, mod$rhs, types = "C",
                   lower = mod$lower, upper = mod$upper,
                   time_limit = 30)$objective
      })
      expect_gte(lp[[2]], lp[[1]] - 1e-6)
      tested <- tested + 1L
      if (lp[[2]] > lp[[1]] + 1e-6) strict <- strict + 1L
    }
  }
  expect_gte(tested, 10L)
  expect_gt(strict, 0L)     # dominance is strict somewhere
})

# ---------------------------------------------------------------------------
# Thm 4b.3' (docs/08): the unit-cost reduction, restored 2026-09-05.
# The piggyback move above is real, but it does not lower the optimum: a
# singleton {u} swaps for any covering set at the same unit cost, and a donor
# pays 1 + m - |K| blanks to credit a group that one set covers. So the gadget
# optimum is the minimum cover after all. These tests pin the statement on the
# instances where an escape would be worth taking; the proof over the whole
# family is in docs/08-theory.md.

test_that("unit-cost gadget: every record starts as a violator", {
  x <- make_cover_gadget(1:3, list(c(1, 2), c(2, 3), 3))
  chk <- ls_check(x, keyVars = names(x), k = 2)
  expect_equal(chk$fk, rep(1, nrow(x)))
  expect_identical(chk$violators, seq_len(nrow(x)))
})

test_that("unit-cost gadget: activating a minimum cover is feasible and costs tau", {
  U <- 1:4; FF <- list(c(1, 2), c(2, 3), c(3, 4), c(1, 4))
  x <- make_cover_gadget(U, FF)
  expect_equal(min_cover(U, FF), 2)
  y <- x
  ## activate T1 = {1,2} and T3 = {3,4}: blank the a-cell of their hubs
  hubs <- (length(U) + 1L):nrow(x)
  y[hubs[1], "a"] <- NA
  y[hubs[3], "a"] <- NA
  expect_true(ls_check(y, keyVars = names(y), k = 2)$ok)
  expect_equal(sum(is.na(y)) - sum(is.na(x)), 2)
})

test_that("unit-cost gadget: the optimum equals the minimum cover", {
  cases <- list(
    ## all sets singletons: tau = n_e, so buying elements one by one TIES with
    ## covering -- the regime where the piggyback move would show up if it could
    list(U = 1:3, FF = list(1, 2, 3)),
    list(U = 1:4, FF = list(1, 2, 3, 4)),
    ## heavy membership overlap: |D(u,v)| = 1 + m - |T(u) cap T(v)| is small,
    ## which is what makes the donor move cheap
    list(U = 1:4, FF = list(1:4, 1:4, c(1, 2), c(3, 4))),
    ## more sets than elements, and a mixed system
    list(U = 1:4, FF = list(1, 2, c(3, 4), c(1, 2), c(1, 3))),
    list(U = 1:4, FF = list(c(1, 2, 3), c(1, 2, 4), c(1, 3, 4), c(2, 3, 4)))
  )
  for (cs in cases) {
    x <- make_cover_gadget(cs$U, cs$FF)
    tau <- min_cover(cs$U, cs$FF)
    o <- ls_optimal(x, keyVars = names(x), k = 2, solver = "highs", time_limit = 120)
    lbl <- paste(vapply(cs$FF, paste, "", collapse = ""), collapse = "|")
    expect_identical(o$status, "optimal", info = lbl)
    expect_equal(o$objective, tau, info = lbl)
    expect_true(ls_check(o$xAnon, keyVars = names(x), k = 2)$ok, info = lbl)
  }
})

test_that("the referee's replication repair changes no optimum", {
  ## R-fold copies make a piggyback cost R instead of 1. Since piggybacking
  ## never beat the cover anyway, the optimum is unmoved -- the repair is not
  ## needed. Same for padding with dummy empty sets against the donor move.
  U <- 1:3; FF <- list(c(1, 2), 3, c(1, 3))
  tau <- min_cover(U, FF)
  for (par in list(c(R = 1, m0 = 1), c(R = 4, m0 = 1), c(R = 1, m0 = 4))) {
    x <- make_cover_gadget(U, FF, R = par[["R"]], m0 = par[["m0"]])
    o <- ls_optimal(x, keyVars = names(x), k = 2, solver = "highs", time_limit = 120)
    expect_equal(o$objective, tau,
                 info = paste("R =", par[["R"]], "m0 =", par[["m0"]]))
  }
})

test_that("no certified per-violator budget: the family that actually works", {
  ## Closure 6.4 / manuscript Prop 4.5. The family used until 2026-09-06,
  ## tuples (1,...,1,j), does NOT have optimum p-1: all records already agree
  ## on p-1 columns, so one blank suffices. The family that does is
  ## (1,j,...,j) -- agreeing on one column, differing on all others.
  wide <- function(p, n) {              # (1,...,1,j): the wrong one
    d <- as.data.frame(matrix("1", n, p), stringsAsFactors = FALSE)
    d[[p]] <- as.character(seq_len(n)); d[] <- lapply(d, factor); d
  }
  narrow <- function(p, n) {            # (1,j,...,j): the right one
    d <- as.data.frame(matrix("1", n, p), stringsAsFactors = FALSE)
    for (j in 2:p) d[[j]] <- as.character(seq_len(n))
    d[] <- lapply(d, factor); d
  }
  for (p in 3:4) {
    ow <- ls_optimal(wide(p, 5), keyVars = paste0("V", seq_len(p)), k = 2,
                     solver = "highs", time_limit = 120)
    expect_equal(ow$objective, 1, info = paste("(1,..,1,j) at p =", p))

    on <- ls_optimal(narrow(p, 5), keyVars = paste0("V", seq_len(p)), k = 2,
                     solver = "highs", time_limit = 120)
    expect_equal(on$objective, p - 1, info = paste("(1,j,..,j) at p =", p))
    ## and the optimum really does empty one record down to the shared column
    expect_equal(max(rowSums(is.na(on$xAnon))), p - 1)
  }
})

test_that("the hub bound needs D_j non-empty, not just hypothesis (13)", {
  ## With D_j = empty the quantifier in (13) is vacuous and the bound would
  ## read <= 0. Witness from the 2026-09-06 review.
  x <- data.frame(a = c(NA, "7", "7"), b = c("5", "8", "8"),
                  stringsAsFactors = TRUE)
  chk <- ls_check(x, keyVars = c("a", "b"), k = 2)
  expect_identical(chk$violators, 1L)
  ## no violator carries an observed value in column a
  expect_length(unique(stats::na.omit(as.character(x$a)[chk$violators])), 0L)
  o <- ls_optimal(x, keyVars = c("a", "b"), k = 2, solver = "highs")
  expect_equal(o$objective, 1)          # not 0
})
