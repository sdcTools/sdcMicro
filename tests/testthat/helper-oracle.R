# --- independent oracle ------------------------------------------------------
# Exhaustive search over every suppression pattern on violator cells. Only
# usable for tiny instances, which is the point: it is the ground truth the
# MILP is checked against, and it shares no code with the model builder.
brute_force_optimum <- function(x, keyVars, k, cost_j = NULL,
                                semantics = "wildcard", alpha = 1,
                                cells = c("violators", "all")) {
  cells <- match.arg(cells)
  V <- ls_check(x, keyVars, k, alpha = alpha, semantics = semantics)$violators
  p <- length(keyVars)
  if (length(V) == 0L) return(0)
  if (is.null(cost_j)) cost_j <- rep(1, p)
  rows <- if (cells == "all") seq_len(nrow(x)) else V
  cells <- expand.grid(i = rows, j = seq_len(p))
  m <- nrow(cells)
  # per-column cost vector, or a full n x p per-cell cost matrix
  cell_cost <- if (is.matrix(cost_j)) cost_j[cbind(cells$i, cells$j)]
               else cost_j[cells$j]
  best <- Inf
  for (mask in 0:(2^m - 1)) {
    sel <- which(bitwAnd(mask, 2L^(seq_len(m) - 1L)) > 0)
    cost <- sum(cell_cost[sel])
    if (cost >= best) next
    y <- x
    for (s in sel) y[cells$i[s], keyVars[cells$j[s]]] <- NA
    if (ls_check(y, keyVars, k, alpha = alpha, semantics = semantics)$ok) best <- cost
  }
  best
}
