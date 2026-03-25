#' Record linkage via Global Distance-Based Record Linkage
#'
#' @description
#' Implements the Global Distance-Based Record Linkage 
#' (GDBRL; Herranz et al., 2015), which links records in an original dataset to 
#' records in an anonymized/protected dataset by computing pairwise distances on 
#' selected linkage variables and then finding the minimum-total-distance 
#' one-to-one matching via the Hungarian algorithm.
#'
#' This corresponds to an attacker scenario in which the adversary knows the
#' original data, or equivalent external information, for the linkage variables
#' and uses the released protected dataset to infer the most plausible global
#' record-to-record matching.
#' 
#' @name recordLinkage
#' @docType methods
#'
#' @param x A `data.frame` containing the original data.
#' @param y A `data.frame` containing the anonymized/protected data.
#' @param vars Character vector of variable names used for record linkage.
#'   These variables must exist in both `x` and `y`.
#' @param distance Character string specifying the distance metric. One of
#'   `"gower"` (default), `"euclidean"`, or `"manhattan"`.
#' @param weights Optional numeric vector of variable weights passed to
#'   [cluster::daisy()]. Must have length `length(vars)`. If `NULL`, equal
#'   weights are used.
#' @param x_id Optional single character string naming the identifier column in
#'   `x`. If `NULL`, row numbers are used as truth IDs.
#' @param y_id Optional single character string naming the identifier column in
#'   `y`. If `NULL`, row numbers are used as truth IDs.
#' @param return_matrix Logical; if `TRUE`, the full pairwise distance matrix is
#'   returned.
#' @param na_action Character string specifying how to handle missing values in
#'   linkage variables. One of:
#'   \describe{
#'     \item{`ignore`}{retain missing values and compute pairwise distances
#'     using the subset of linkage variables observed for each record pair, as
#'     handled by [cluster::daisy()]. Distances for different record pairs may 
#'     therefore be based on different numbers of variables. Missing values are 
#'     not treated as a separate category and do not contribute directly to the 
#'     corresponding variable-specific distance.}
#'     \item{`fail`}{stop if linkage variables contain any missing values.}
#'   }
#' @param tol Numeric tolerance used to determine tied minimum distances.
#'
#' @return An object of class `"recordLinkage"` with elements:
#' \describe{
#'   \item{matches}{A data.frame with matched pairs and corresponding distances}
#'   \item{correct_matches}{Number of correctly linked records}
#'   \item{correct_match_rate}{Proportion of correctly linked records}
#'   \item{mean_distance}{Mean matched distance}
#'   \item{total_distance}{Total matched distance}
#'   \item{distance_matrix}{Optional full pairwise distance matrix}
#'   \item{call}{The matched call}
#' }
#'
#' @details
#' The distance measure can be chosen via `distance`. Gower distance is suitable 
#' for mixed-type quasi-identifiers, including numeric, factor, character, and 
#' logical variables. Variables of class factor are treated as nominal variables, 
#' while variables of class ordered are treated as ordinal variables.
#' Euclidean and Manhattan distances are supported for purely 
#' numeric linkage variables. The Hungarian algorithm finds the global 
#' minimum-cost one-to-one assignment. 
#' 
#' In addition to the global assignment, the function also returns the number of 
#' candidates attaining the minimum distance (`n_best`). The quantity 
#' `n_best` counts, for each record in `x`, how many records in `y` attain the 
#' same minimum row-wise distance in the pairwise distance matrix. If multiple 
#' optimal assignments exist, the chosen solution depends on the deterministic 
#' behavior of [clue::solve_LSAP()] for the supplied cost matrix. 
#' 
#' For strict global assignment, `nrow(x)` must equal `nrow(y)`.
#' If `x_id` and `y_id` are not supplied, row order is treated as the truth for
#' evaluating correct matches. 
#' 
#' Results depend on both the matching direction and the row order of the input 
#' data frames.
#'
#' @references
#' 
#' Herranz, J., Nin, J., Rodríguez, P., and Tassa, T. (2015). \emph{Revisiting
#' distance-based record linkage for privacy-preserving release of statistical
#' datasets}. Data & Knowledge Engineering, 100, 78--93.
#' \doi{10.1016/j.datak.2015.07.009}
#' 
#' Hornik, K. (2005). \emph{A CLUE for cluster ensembles}.
#' Journal of Statistical Software, 14(12).
#' \doi{10.18637/jss.v014.i12}
#' 
#' Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., & Hornik, K. (2026).
#' \emph{cluster: Cluster Analysis Basics and Extensions}.
#' \url{https://CRAN.R-project.org/package=cluster}
#'
#' @examples
#' x <- data.frame(
#'   id = c(1, 2, 3),
#'   age = c(23, 40, 35),
#'   sex = factor(c("f", "m", "f")),
#'   region = c("A", "B", "A"),
#'   stringsAsFactors = FALSE
#' )
#'
#' y <- data.frame(
#'   id = c(1, 2, 3),
#'   age = c(24, 39, 35),
#'   sex = factor(c("f", "m", "f")),
#'   region = c("A", "B", "B"),
#'   stringsAsFactors = FALSE
#' )
#'
#' out <- recordLinkage(
#'   x = x,
#'   y = y,
#'   vars = c("age", "sex", "region"),
#'   distance = "gower",
#'   x_id = "id",
#'   y_id = "id"
#' )
#'
#' out
#' out$matches
#'
#' @export
recordLinkage <- function(x,
                          y,
                          vars,
                          distance = c("gower", "euclidean", "manhattan"),
                          weights = NULL,
                          x_id = NULL,
                          y_id = NULL,
                          return_matrix = FALSE,
                          na_action = c("ignore", "fail"),
                          tol = sqrt(.Machine$double.eps)) {

  distance <- match.arg(distance)
  na_action <- match.arg(na_action)

  # input checks ---------------------------------------------------------------
  if (!is.data.frame(x)) {
    stop("`x` must be a data.frame.", call. = FALSE)
  }
  if (!is.data.frame(y)) {
    stop("`y` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(vars) || length(vars) == 0L) {
    stop("`vars` must be a non-empty character vector.", call. = FALSE)
  }
  if (!all(vars %in% names(x))) {
    missing_x <- vars[!vars %in% names(x)]
    stop("Variables not found in `x`: ", paste(missing_x, collapse = ", "),
         call. = FALSE)
  }
  if (!all(vars %in% names(y))) {
    missing_y <- vars[!vars %in% names(y)]
    stop("Variables not found in `y`: ", paste(missing_y, collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(vars)) {
    stop("`vars` must not contain duplicate variable names.", call. = FALSE)
  }
  if (nrow(x) != nrow(y)) {
    stop("`x` and `y` must have the same number of rows for strict one-to-one ",
         "Hungarian assignment.", call. = FALSE)
  }
  if (!is.logical(return_matrix) || length(return_matrix) != 1L || is.na(return_matrix)) {
    stop("`return_matrix` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol < 0) {
    stop("`tol` must be a single non-negative finite number.", call. = FALSE)
  }
  if (!requireNamespace("clue", quietly = TRUE)) {
    stop("Package 'clue' is required for this function.", call. = FALSE)
  }
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package 'cluster' is required for this function.", call. = FALSE)
  }

  # identifier handling --------------------------------------------------------
  if (is.null(x_id)) {
    x_ids <- seq_len(nrow(x))
  } else {
    if (!is.character(x_id) || length(x_id) != 1L) {
      stop("`x_id` must be NULL or a single column name.", call. = FALSE)
    }
    if (!x_id %in% names(x)) {
      stop("`x_id` not found in `x`.", call. = FALSE)
    }
    x_ids <- x[[x_id]]
  }

  if (is.null(y_id)) {
    y_ids <- seq_len(nrow(y))
  } else {
    if (!is.character(y_id) || length(y_id) != 1L) {
      stop("`y_id` must be NULL or a single column name.", call. = FALSE)
    }
    if (!y_id %in% names(y)) {
      stop("`y_id` not found in `y`.", call. = FALSE)
    }
    y_ids <- y[[y_id]]
  }

  if (length(x_ids) != nrow(x)) {
    stop("Identifier column `x_id` must have length nrow(x).", call. = FALSE)
  }
  if (length(y_ids) != nrow(y)) {
    stop("Identifier column `y_id` must have length nrow(y).", call. = FALSE)
  }
  
  if (anyNA(x_ids) || anyNA(y_ids)) {
    warning(
      "Missing values found in `x_id` or `y_id`; correctness of matched pairs ",
      "cannot be fully evaluated for those records.",
      call. = FALSE
    )
  }

  # weights --------------------------------------------------------------------
  if (is.null(weights)) {
    weights <- rep(1, length(vars))
  } else {
    if (!is.numeric(weights) || length(weights) != length(vars)) {
      stop("`weights` must be numeric with length equal to `length(vars)`.",
           call. = FALSE)
    }
    if (any(!is.finite(weights)) || any(weights < 0)) {
      stop("`weights` must contain finite non-negative values.", call. = FALSE)
    }
    if (sum(weights) == 0) {
      stop("At least one weight must be positive.", call. = FALSE)
    }
  }

  # subset linkage variables ---------------------------------------------------
  x_sub <- x[, vars, drop = FALSE]
  y_sub <- y[, vars, drop = FALSE]
  
  harmonized <- .harmonize_linkage_data(x_sub, y_sub)
  x_sub <- harmonized$x
  y_sub <- harmonized$y

  # metric-specific checks -----------------------------------------------------
  if (distance %in% c("euclidean", "manhattan")) {
    is_numeric_like <- vapply(
      x_sub,
      function(z) is.numeric(z) || is.integer(z) || is.logical(z),
      logical(1L)
    ) &
      vapply(
        y_sub,
        function(z) is.numeric(z) || is.integer(z) || is.logical(z),
        logical(1L)
      )

    if (!all(is_numeric_like)) {
      bad_vars <- vars[!is_numeric_like]
      stop(
        "For `distance = '", distance, "'`, all linkage variables must be ",
        "numeric, integer, or logical. Non-numeric variables found: ",
        paste(bad_vars, collapse = ", "),
        ". Use `distance = 'gower'` for mixed-type data.",
        call. = FALSE
      )
    }

    x_sub[] <- lapply(x_sub, function(z) as.numeric(z))
    y_sub[] <- lapply(y_sub, function(z) as.numeric(z))
  }

  # missingness handling -------------------------------------------------------
  if (identical(na_action, "fail")) {
    if (anyNA(x_sub) || anyNA(y_sub)) {
      stop("Missing values found in linkage variables and `na_action = 'fail'`.",
           call. = FALSE)
    }
  }

  # pairwise distance matrix ---------------------------------------------------
  combined <- rbind(x_sub, y_sub)

  d_all <- cluster::daisy(
    x = combined,
    metric = distance,
    weights = weights
  )

  d_mat <- as.matrix(d_all)
  n_x <- nrow(x_sub)
  n_y <- nrow(y_sub)

  cost <- d_mat[seq_len(n_x), n_x + seq_len(n_y), drop = FALSE]
  
  dimnames(cost) <- list(seq_len(n_x), seq_len(n_y))

  if (any(!is.finite(cost))) {
    stop(
      "Non-finite entries found in the pairwise distance matrix. ",
      "This can occur when missing linkage values prevent distance computation ",
      "for one or more record pairs.",
      call. = FALSE
    )
  }
  
  # tied best candidates based on cost matrix ----------------------------------
  row_min <- apply(cost, 1L, min)

  n_best <- vapply(
    seq_len(nrow(cost)),
    function(i) {
      sum(abs(cost[i, ] - row_min[i]) <= tol)
    },
    integer(1L)
  )

  # Hungarian assignment -------------------------------------------------------
  assignment <- clue::solve_LSAP(cost, maximum = FALSE)

  matched_y_index <- as.integer(assignment)
  matched_cost <- cost[cbind(seq_len(n_x), matched_y_index)]

  matched_x_ids <- x_ids
  matched_y_ids <- y_ids[matched_y_index]

  correct <- matched_x_ids == matched_y_ids

  matches <- data.frame(
    x_row = seq_len(n_x),
    y_row = matched_y_index,
    x_id = matched_x_ids,
    y_id = matched_y_ids,
    distance = matched_cost,
    correct_match = correct,
    n_best = n_best,
    stringsAsFactors = FALSE
  )

  correct_matches <- sum(correct, na.rm = TRUE)
  
  n_evaluable <- sum(!is.na(correct))
  
  correct_match_rate <- if (n_evaluable > 0L) {
    mean(correct, na.rm = TRUE)
  } else {
    NA_real_
  }

  out <- list(
    matches = matches,
    correct_matches = correct_matches,
    correct_match_rate = correct_match_rate,
    mean_distance = mean(matched_cost),
    total_distance = sum(matched_cost),
    call = match.call()
  )

  if (isTRUE(return_matrix)) {
    out$distance_matrix <- cost
  }

  class(out) <- "recordLinkage"
  out
}

# harmonize variable classes across files ------------------------------------
#' Harmonize linkage variables across two data.frames
#'
#' Internal helper that coerces corresponding variables in two data.frames to
#' compatible classes for distance computation.
#'
#' @param x A data.frame.
#' @param y A data.frame.
#'
#' @return A list with harmonized `x` and `y`.
#' @noRd
.harmonize_linkage_data <- function(x, y) {
  
  out_x <- x
  out_y <- y
  
  for (j in seq_along(out_x)) {
    xj <- out_x[[j]]
    yj <- out_y[[j]]
    var_name <- names(out_x)[j]
    
    # ordered factors --------------------------------------------------------
    if (is.ordered(xj) || is.ordered(yj)) {
      
      # both ordered: require identical level order
      if (is.ordered(xj) && is.ordered(yj)) {
        lev_x <- levels(xj)
        lev_y <- levels(yj)
        
        if (!identical(lev_x, lev_y)) {
          stop(
            "Ordered factor levels for variable `", var_name,
            "` are not identical in `x` and `y`.",
            call. = FALSE
          )
        }
        
        lev <- lev_x
        
        # one ordered, one unordered/character: use ordered levels as reference
      } else if (is.ordered(xj)) {
        lev <- levels(xj)
        
        extra_y <- setdiff(unique(as.character(yj[!is.na(yj)])), lev)
        if (length(extra_y) > 0L) {
          stop(
            "Variable `", var_name, "` is ordered in `x`, but `y` contains ",
            "values not present in the ordered levels: ",
            paste(extra_y, collapse = ", "),
            call. = FALSE
          )
        }
        
      } else {  # is.ordered(yj)
        lev <- levels(yj)
        
        extra_x <- setdiff(unique(as.character(xj[!is.na(xj)])), lev)
        if (length(extra_x) > 0L) {
          stop(
            "Variable `", var_name, "` is ordered in `y`, but `x` contains ",
            "values not present in the ordered levels: ",
            paste(extra_x, collapse = ", "),
            call. = FALSE
          )
        }
      }
      
      out_x[[j]] <- ordered(as.character(xj), levels = lev)
      out_y[[j]] <- ordered(as.character(yj), levels = lev)
      
      # unordered factor / character -----------------------------------------
    } else if (is.factor(xj) || is.factor(yj) ||
               is.character(xj) || is.character(yj)) {
      
      lev <- union(as.character(unique(xj)), as.character(unique(yj)))
      lev <- lev[!is.na(lev)]
      
      out_x[[j]] <- factor(as.character(xj), levels = lev)
      out_y[[j]] <- factor(as.character(yj), levels = lev)
      
      # logical --------------------------------------------------------------
    } else if (is.logical(xj) || is.logical(yj)) {
      out_x[[j]] <- as.logical(xj)
      out_y[[j]] <- as.logical(yj)
      
      # numeric / integer ----------------------------------------------------
    } else if ((is.numeric(xj) || is.integer(xj)) &&
               (is.numeric(yj) || is.integer(yj))) {
      out_x[[j]] <- as.numeric(xj)
      out_y[[j]] <- as.numeric(yj)
      
    } else {
      stop(
        "Unsupported or incompatible variable classes for variable `",
        var_name, "`: ",
        paste(class(xj), collapse = "/"), " vs ",
        paste(class(yj), collapse = "/"),
        call. = FALSE
      )
    }
  }
  
  list(x = out_x, y = out_y)
}

#' @method print recordLinkage
#' @export
print.recordLinkage <- function(x, digits = 2, ...) {
  n_evaluable <- sum(!is.na(x$matches$correct_match))
  
  cat("<recordLinkage>\n")
  cat("Correct matches:       ",
      x$correct_matches, "/", n_evaluable, "\n", sep = "")
  cat("Correct match percent: ",
      format(round(x$correct_match_rate * 100, digits), nsmall = digits), "%\n", sep = "")
  cat("Mean distance:         ",
      format(x$mean_distance, digits = 6), "\n", sep = "")
  invisible(x)
}