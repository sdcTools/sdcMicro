## Set-Cover gadget of docs/08-theory.md Thm 4b.3', with the two ablation knobs
## that were tested and found unnecessary: R-fold replication of the elements
## (the EJOR referee's proposed repair against the piggyback move) and m0 dummy
## empty sets (against the donor move). R = m0 = 1 is the theorem's gadget.
make_cover_gadget <- function(U, FF, R = 1L, m0 = 1L) {
  m <- length(FF)
  cols <- c("a", paste0("s", seq_len(m)), if (m0 > 0) paste0("d", seq_len(m0)))
  blank <- stats::setNames(rep(NA_character_, length(cols)), cols)
  rows <- list()
  for (u in U) for (i in seq_len(R)) {
    r <- blank
    r["a"] <- "e"
    for (t in seq_len(m))
      r[paste0("s", t)] <- if (u %in% FF[[t]]) "1" else paste0("q_", u, "_", i, "_s", t)
    if (m0 > 0) for (j in seq_len(m0))
      r[paste0("d", j)] <- paste0("q_", u, "_", i, "_d", j)
    rows[[length(rows) + 1L]] <- r
  }
  hub <- function(col, off) { r <- blank; r["a"] <- off; r[col] <- "1"; r }
  for (t in seq_len(m)) rows[[length(rows) + 1L]] <- hub(paste0("s", t), paste0("off_s", t))
  if (m0 > 0) for (j in seq_len(m0))
    rows[[length(rows) + 1L]] <- hub(paste0("d", j), paste0("off_d", j))
  x <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  x[] <- lapply(x, factor)
  x
}

## minimum cover by brute force
min_cover <- function(U, FF) {
  for (sz in seq_along(FF)) for (S in utils::combn(length(FF), sz, simplify = FALSE))
    if (all(U %in% unlist(FF[S]))) return(sz)
  NA_integer_
}
