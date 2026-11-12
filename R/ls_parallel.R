# Parallel computation of strata in localSuppression()/kAnon() (issue #349).
# Base package 'parallel' only: forked processes where fork exists (Linux,
# macOS), a PSOCK cluster on Windows.

# lapply() over X, on a cluster if nc > 1 and X has more than one element.
#
# X        list (one element per stratum)
# FUN, ... function applied to each element of X, and further arguments to it.
#          For a PSOCK cluster FUN is sent to fresh R sessions: it should be a
#          function of the package (the workers then load the installed
#          sdcMicro), not a closure dragging a large environment along.
# nc       number of workers
# cl_type  "auto" picks "PSOCK" on Windows and "FORK" elsewhere
#
# The result is in the order of X and equal to lapply(X, FUN, ...). An error in
# a worker is an error here, a warning in a worker is a warning here (each
# distinct message once). FUN must not rely on the random number stream of the
# calling session: the workers have their own. Each worker uses one data.table thread and one
# MILP solver thread (see ls_milp_threads()), otherwise nc workers times the
# threads of each would oversubscribe the cores.
ls_lapply <- function(X, FUN, ..., nc = 1, cl_type = c("auto", "FORK", "PSOCK")) {
  cl_type <- match.arg(cl_type)
  nc <- min(nc, length(X))
  if (nc <= 1) {
    return(lapply(X, FUN, ...))
  }
  if (cl_type == "auto") {
    cl_type <- if (.Platform$OS.type == "windows") "PSOCK" else "FORK"
  }
  if (cl_type == "FORK" && .Platform$OS.type == "windows") {
    stop("a fork cluster is not available on Windows.", call. = FALSE)
  }
  # warnings raised in a worker are collected and raised again in the calling
  # session; otherwise they would be lost
  worker <- function(x, .fun, .args) {
    data.table::setDTthreads(1L)
    options(sdcMicro.milp_threads = 1L)
    warn <- character(0)
    value <- withCallingHandlers(
      do.call(.fun, c(list(x), .args)),
      warning = function(w) {
        warn <<- c(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warn)
  }
  # the wrapper itself needs nothing from this package
  environment(worker) <- globalenv()
  args <- list(...)
  collect <- function(res) {
    for (w in unique(unlist(lapply(res, `[[`, "warnings")))) {
      warning(w, call. = FALSE)
    }
    lapply(res, `[[`, "value")
  }

  if (cl_type == "FORK") {
    msgs <- character(0)
    res <- withCallingHandlers(
      parallel::mclapply(X, worker, .fun = FUN, .args = args, mc.cores = nc),
      warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    # mclapply() turns an error in a child into a "try-error" result plus a warning
    failed <- vapply(res, inherits, logical(1), what = "try-error")
    if (any(failed)) {
      cond <- attr(res[[which(failed)[1]]], "condition")
      stop(if (is.null(cond)) as.character(res[[which(failed)[1]]]) else conditionMessage(cond),
           call. = FALSE)
    }
    if (length(msgs) > 0) {
      stop("parallel computation of the strata failed: ", paste(unique(msgs), collapse = "; "),
           call. = FALSE)
    }
    return(collect(res))
  }

  cl <- parallel::makeCluster(nc, type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  res <- tryCatch(
    parallel::parLapply(cl, X, worker, .fun = FUN, .args = args),
    error = function(e) stop(conditionMessage(e), call. = FALSE)
  )
  collect(res)
}

# Number of threads a MILP backend may use, from option 'sdcMicro.milp_threads';
# NULL if the option is not set (the backend's own default then applies).
# ls_lapply() sets the option to 1 in its workers. Only 'gurobi' needs it: the
# R interfaces of 'highs' and 'scip' run on one thread by default.
ls_milp_threads <- function() {
  th <- getOption("sdcMicro.milp_threads")
  if (is.null(th)) {
    return(NULL)
  }
  if (!is.numeric(th) || length(th) != 1L || is.na(th) || th < 1) {
    stop("option 'sdcMicro.milp_threads' must be a single positive integer.", call. = FALSE)
  }
  as.integer(th)
}

# One stratum of localSuppressionWORK() when k-anonymity is requested for
# subsets of the key variables ('combs'): all combinations in turn.
# d: data.table of the stratum (key variables first, then 'strata', 'sortid');
# tree: list of combn() matrices; use_importance: hand 'importance' to the engine?
ls_stratum_combs <- function(d, tree, k, importance, use_importance, alpha, method, control) {
  sortid <- NULL
  tmpDat <- copy(d)
  info <- list()
  for (gr in seq_along(tree)) {
    cur_k <- k[gr]
    for (comb in seq_len(ncol(tree[[gr]]))) {
      # indices within the key variables, which are the leading columns of 'd'
      kV <- tree[[gr]][, comb]
      cur_importance <- rank(importance[kV], ties.method = "min")
      res <- suppSubset(
        x = tmpDat[, kV, with = FALSE],
        k = cur_k,
        importance = if (use_importance) cur_importance else NULL,
        alpha = alpha,
        method = method,
        control = control
      )
      info[[length(info) + 1]] <- res$info
      for (z in seq_along(kV)) {
        set(tmpDat, i = NULL, j = kV[z], res$xAnon[[z]])
      }
    }
  }
  tmpDat[, sortid := NULL]
  list(xAnon = tmpDat, info = info)
}
