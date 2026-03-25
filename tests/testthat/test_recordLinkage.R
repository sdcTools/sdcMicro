# Tests for recordLinkage Function

# load library
library(testthat)

testthat::skip_if_not_installed("clue")
testthat::skip_if_not_installed("cluster")

test_that("recordLinkage returns correct match rate (example 1 from Harrenz et al. (2015))", {
  x <- data.frame(
    v1 = c(1, 0, -1, 0),
    v2 = c(0, 1, 0, -1),
    id = 1:4
  )
  
  y <- data.frame(
    v1 = c(0, 0, -2.1, 0),
    v2 = c(0, 2.1, 0, -2.1),
    id = 1:4
  )
  
  out <- recordLinkage(x, 
                       y,
                       x_id = "id",
                       y_id = "id",
                       na_action = "ignore",
                       return_matrix = TRUE,
                       vars = c("v1", "v2"))
  
  expect_equal(out$correct_match_rate, 1)
})

test_that("recordLinkage returns correct match rate (example 2 from Harrenz et al. (2015))", {
  x <- data.frame(
    v1 = c(1, 2, 3, 4),
    id = 1:4
  )
  
  y <- data.frame(
    v1 = c(2, 3, 4, -0.1),
    id = 1:4
  )
  
  out <- recordLinkage(x, 
                       y,
                       x_id = "id",
                       y_id = "id",
                       na_action = "ignore",
                       return_matrix = TRUE,
                       vars = c("v1"))
  
  expect_equal(out$correct_match_rate, 0)
})

test_that("gower distance matrix is computed as expected for mixed-type linkage variables", {
  x <- data.frame(
    id = 1:2,
    age = c(20, 40),
    sex = factor(c("f", "m")),
    region = c("A", "B"),
    stringsAsFactors = FALSE
  )
  
  y <- data.frame(
    id = 1:2,
    age = c(20, 30),
    sex = factor(c("f", "m")),
    region = c("B", "B"),
    stringsAsFactors = FALSE
  )
  
  out <- recordLinkage(
    x = x,
    y = y,
    vars = c("age", "sex", "region"),
    distance = "gower",
    x_id = "id",
    y_id = "id",
    return_matrix = TRUE,
    tol = 0
  )
  
  expect_true(is.matrix(out$distance_matrix))
  expect_equal(dim(out$distance_matrix), c(2, 2))
  
  # Manual Gower distances:
  # age range over combined data = 40 - 20 = 20
  #
  # x1=(20,f,A) vs y1=(20,f,B):
  #   age    = 0/20   = 0
  #   sex    = 0
  #   region = 1
  #   mean   = (0 + 0 + 1) / 3 = 1/3
  #
  # x1=(20,f,A) vs y2=(30,m,B):
  #   age    = 10/20  = 0.5
  #   sex    = 1
  #   region = 1
  #   mean   = (0.5 + 1 + 1) / 3 = 5/6
  #
  # x2=(40,m,B) vs y1=(20,f,B):
  #   age    = 20/20  = 1
  #   sex    = 1
  #   region = 0
  #   mean   = 2/3
  #
  # x2=(40,m,B) vs y2=(30,m,B):
  #   age    = 10/20  = 0.5
  #   sex    = 0
  #   region = 0
  #   mean   = 1/6
  expected <- matrix(
    c(
      1/3, 5/6,
      2/3, 1/6
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("1", "2"), c("1", "2"))
  )
  
  expect_equal(out$distance_matrix, expected, tolerance = 1e-8)
  
  # Optimal assignment should be x1->y1 and x2->y2
  expect_equal(out$matches$y_row, c(1L, 2L))
  expect_true(all(out$matches$correct_match))
  expect_equal(out$correct_match_rate, 1)
})

test_that("weighted gower distance matrix is computed as expected for mixed-type linkage variables", {
  x <- data.frame(
    id = 1:2,
    age = c(20, 40),
    sex = factor(c("f", "m")),
    region = c("A", "B"),
    stringsAsFactors = FALSE
  )
  
  y <- data.frame(
    id = 1:2,
    age = c(20, 30),
    sex = factor(c("f", "m")),
    region = c("B", "B"),
    stringsAsFactors = FALSE
  )
  
  w <- c(age = 2, sex = 1, region = 1)
  
  out <- recordLinkage(
    x = x,
    y = y,
    vars = c("age", "sex", "region"),
    distance = "gower",
    weights = w,
    x_id = "id",
    y_id = "id",
    return_matrix = TRUE,
    tol = 0
  )
  
  expect_true(is.matrix(out$distance_matrix))
  expect_equal(dim(out$distance_matrix), c(2, 2))
  
  # Manual weighted Gower distances:
  # age range over combined data = 40 - 20 = 20
  # weights sum = 2 + 1 + 1 = 4
  #
  # x1=(20,f,A) vs y1=(20,f,B):
  # age    = 0/20   = 0,   weighted = 2*0
  # sex    = 0,            weighted = 1*0
  # region = 1,            weighted = 1*1
  # total = (0 + 0 + 1) / 4 = 1/4
  #
  # x1=(20,f,A) vs y2=(30,m,B):
  # age    = 10/20  = 0.5, weighted = 2*0.5 = 1
  # sex    = 1,            weighted = 1
  # region = 1,            weighted = 1
  # total = (1 + 1 + 1) / 4 = 3/4
  #
  # x2=(40,m,B) vs y1=(20,f,B):
  # age    = 20/20  = 1,   weighted = 2
  # sex    = 1,            weighted = 1
  # region = 0,            weighted = 0
  # total = (2 + 1 + 0) / 4 = 3/4
  #
  # x2=(40,m,B) vs y2=(30,m,B):
  # age    = 10/20  = 0.5, weighted = 1
  # sex    = 0,            weighted = 0
  # region = 0,            weighted = 0
  # total = (1 + 0 + 0) / 4 = 1/4
  
  expected <- matrix(
    c(1/4, 3/4,
      3/4, 1/4),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("1", "2"), c("1", "2"))
  )
  
  expect_equal(out$distance_matrix, expected, tolerance = 1e-8)
  
  # Optimal assignment should be x1->y1 and x2->y2
  expect_equal(out$matches$y_row, c(1L, 2L))
  expect_true(all(out$matches$correct_match))
  expect_equal(out$correct_match_rate, 1)
})

test_that("Hungarian assignment finds the global minimum one-to-one matching", {
  x <- data.frame(
    id = 1:3,
    a = c(0, 2, 100)
  )
  
  y <- data.frame(
    id = 1:3,
    a = c(0, 100, 101)
  )
  
  out <- recordLinkage(
    x = x,
    y = y,
    vars = "a",
    distance = "euclidean",
    x_id = "id",
    y_id = "id",
    return_matrix = TRUE
  )
  
  # Cost matrix should be:
  #          y1   y2   y3
  # x1(0)     0  100  101
  # x2(2)     2   98   99
  # x3(100) 100    0    1
  
  expected <- matrix(
    c(
      0, 100, 101,
      2,  98,  99,
      100, 0,   1
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("1", "2", "3"), c("1", "2", "3"))
  )
  
  expect_equal(out$distance_matrix, expected, tolerance = 1e-8)
  
  # Greedy row-wise would pick x1->y1, x2->y1, x3->y2.
  # Under one-to-one assignment, the global optimum is:
  #   x1 -> y1  (0)
  #   x2 -> y3  (99)
  #   x3 -> y2  (0)
  # total = 99
  #
  # Alternative feasible assignment x1->y1, x2->y2, x3->y3 totals 99 as well.
  # So the key property to test is that:
  #   1) assignment is one-to-one
  #   2) total distance is globally minimal
  expect_equal(sort(out$matches$y_row), 1:3)
  expect_equal(out$total_distance, 99, tolerance = 1e-8)
})

test_that("recordLinkage returns expected structure for a simple gower match", {
  x <- data.frame(
    id = c(1, 2, 3),
    age = c(23, 40, 35),
    sex = factor(c("f", "m", "f")),
    region = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )

  y <- data.frame(
    id = c(1, 2, 3),
    age = c(24, 39, 35),
    sex = factor(c("f", "m", "f")),
    region = c("A", "B", "B"),
    stringsAsFactors = FALSE
  )

  out <- recordLinkage(
    x = x,
    y = y,
    vars = c("age", "sex", "region"),
    distance = "gower",
    x_id = "id",
    y_id = "id"
  )

  expect_s3_class(out, "recordLinkage")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "matches",
      "correct_matches",
      "correct_match_rate",
      "mean_distance",
      "total_distance",
      "call"
    ),
    ignore.order = TRUE
  )

  expect_s3_class(out$matches, "data.frame")
  expect_equal(
    names(out$matches),
    c("x_row", "y_row", "x_id", "y_id", "distance", "correct_match", "n_best")
  )
  expect_equal(nrow(out$matches), 3L)

  expect_true(is.numeric(out$correct_matches))
  expect_true(is.numeric(out$correct_match_rate))
  expect_true(is.numeric(out$mean_distance))
  expect_true(is.numeric(out$total_distance))

  expect_equal(out$correct_matches, sum(out$matches$correct_match, na.rm = TRUE))
  expect_equal(out$correct_match_rate, mean(out$matches$correct_match, na.rm = TRUE))
  expect_equal(out$mean_distance, mean(out$matches$distance))
  expect_equal(out$total_distance, sum(out$matches$distance))
})

test_that("recordLinkage uses row order as truth when x_id and y_id are NULL", {
  x <- data.frame(a = c(1, 2), b = c("x", "y"), stringsAsFactors = FALSE)
  y <- data.frame(a = c(1, 2), b = c("x", "y"), stringsAsFactors = FALSE)

  out <- recordLinkage(
    x = x,
    y = y,
    vars = c("a", "b")
  )

  expect_equal(out$matches$x_id, 1:2)
  expect_equal(out$matches$y_id, 1:2)
  expect_true(all(out$matches$correct_match))
  expect_equal(out$correct_matches, 2)
  expect_equal(out$correct_match_rate, 1)
})

test_that("recordLinkage returns distance_matrix when requested", {
  x <- data.frame(id = 1:3, v = c(1, 2, 3))
  y <- data.frame(id = 1:3, v = c(1, 2, 4))

  out <- recordLinkage(
    x = x,
    y = y,
    vars = "v",
    distance = "euclidean",
    x_id = "id",
    y_id = "id",
    return_matrix = TRUE
  )

  expect_true("distance_matrix" %in% names(out))
  expect_true(is.matrix(out$distance_matrix))
  expect_equal(dim(out$distance_matrix), c(3, 3))

  expect_equal(
    out$matches$distance,
    out$distance_matrix[cbind(seq_len(nrow(x)), out$matches$y_row)]
  )
})

test_that("recordLinkage validates x and y as data.frames", {
  y <- data.frame(a = 1)

  expect_error(
    recordLinkage(x = 1, y = y, vars = "a"),
    "`x` must be a data.frame.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x = y, y = 1, vars = "a"),
    "`y` must be a data.frame.",
    fixed = TRUE
  )
})

test_that("recordLinkage validates vars", {
  x <- data.frame(a = 1:2)
  y <- data.frame(a = 1:2)

  expect_error(
    recordLinkage(x, y, vars = NULL),
    "`vars` must be a non-empty character vector.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = character()),
    "`vars` must be a non-empty character vector.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = 1),
    "`vars` must be a non-empty character vector.",
    fixed = TRUE
  )
})

test_that("recordLinkage errors when vars are missing from x or y", {
  x <- data.frame(a = 1:2, b = 3:4)
  y <- data.frame(a = 1:2, c = 3:4)

  expect_error(
    recordLinkage(x, y, vars = c("a", "c")),
    "Variables not found in `x`: c",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = c("a", "b")),
    "Variables not found in `y`: b",
    fixed = TRUE
  )
})

test_that("recordLinkage errors when x and y have different number of rows", {
  x <- data.frame(a = 1:2)
  y <- data.frame(a = 1:3)

  expect_error(
    recordLinkage(x, y, vars = "a"),
    "`x` and `y` must have the same number of rows for strict one-to-one Hungarian assignment.",
    fixed = TRUE
  )
})

test_that("recordLinkage validates tol", {
  x <- data.frame(a = 1:2)
  y <- data.frame(a = 1:2)

  expect_error(
    recordLinkage(x, y, vars = "a", tol = -1),
    "`tol` must be a single non-negative finite number.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = "a", tol = c(1, 2)),
    "`tol` must be a single non-negative finite number.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = "a", tol = NA_real_),
    "`tol` must be a single non-negative finite number.",
    fixed = TRUE
  )
})

test_that("recordLinkage validates x_id and y_id", {
  x <- data.frame(id = 1:2, a = 1:2)
  y <- data.frame(id = 1:2, a = 1:2)

  expect_error(
    recordLinkage(x, y, vars = "a", x_id = 1),
    "`x_id` must be NULL or a single column name.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = "a", y_id = 1),
    "`y_id` must be NULL or a single column name.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = "a", x_id = "zzz"),
    "`x_id` not found in `x`.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = "a", y_id = "zzz"),
    "`y_id` not found in `y`.",
    fixed = TRUE
  )
})

test_that("recordLinkage validates weights", {
  x <- data.frame(a = 1:2, b = 3:4)
  y <- data.frame(a = 1:2, b = 3:4)

  expect_error(
    recordLinkage(x, y, vars = c("a", "b"), weights = 1),
    "`weights` must be numeric with length equal to `length(vars)`.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = c("a", "b"), weights = c(1, NA)),
    "`weights` must contain finite non-negative values.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = c("a", "b"), weights = c(1, -1)),
    "`weights` must contain finite non-negative values.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = c("a", "b"), weights = c(0, 0)),
    "At least one weight must be positive.",
    fixed = TRUE
  )
})

test_that("recordLinkage respects na_action = 'fail'", {
  x <- data.frame(a = c(1, NA))
  y <- data.frame(a = c(1, 2))

  expect_error(
    recordLinkage(x, y, vars = "a", na_action = "fail"),
    "Missing values found in linkage variables and `na_action = 'fail'`.",
    fixed = TRUE
  )
})

test_that("recordLinkage allows NA values when na_action = 'ignore' if pairwise distances remain computable", {
  x <- data.frame(
    id = 1:2,
    a = c(1, NA),
    b = factor(c("x", "y"))
  )
  
  y <- data.frame(
    id = 1:2,
    a = c(1, NA),
    b = factor(c("x", "y"))
  )
  
  out <- recordLinkage(
    x, y,
    vars = c("a", "b"),
    x_id = "id",
    y_id = "id",
    na_action = "ignore"
  )
  
  expect_s3_class(out, "recordLinkage")
  expect_equal(nrow(out$matches), 2L)
  expect_true(all(is.finite(out$matches$distance)))
})

test_that("correct_match is NA when truth IDs cannot be evaluated", {
  x <- data.frame(
    id = c(1, NA, 3),
    age = c(20, 30, 40),
    sex = factor(c("f", "m", "f"))
  )
  
  y <- data.frame(
    id = c(1, 2, 3),
    age = c(20, 30, 40),
    sex = factor(c("f", "m", "f"))
  )
  
  out <- suppressWarnings(
    recordLinkage(
      x = x,
      y = y,
      vars = c("age", "sex"),
      distance = "gower",
      x_id = "id",
      y_id = "id",
      na_action = "ignore"
    )
  )
  
  expect_true(anyNA(out$matches$correct_match))
  expect_equal(out$correct_matches, sum(out$matches$correct_match, na.rm = TRUE))
})

test_that("correct_match_rate is NA if no matched pairs are evaluable", {
  x <- data.frame(
    id = c(NA, NA),
    age = c(20, 30)
  )
  
  y <- data.frame(
    id = c(NA, NA),
    age = c(20, 30)
  )
  
  out <- suppressWarnings(
    recordLinkage(
      x = x,
      y = y,
      vars = "age",
      distance = "gower",
      x_id = "id",
      y_id = "id",
      na_action = "ignore"
    )
  )
  
  expect_true(all(is.na(out$matches$correct_match)))
  expect_true(is.na(out$correct_match_rate))
  expect_equal(out$correct_matches, 0)
})

test_that("euclidean and manhattan require numeric/integer/logical linkage variables", {
  x <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  y <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)

  expect_error(
    recordLinkage(x, y, vars = c("a", "b"), distance = "euclidean"),
    "Use `distance = 'gower'` for mixed-type data.",
    fixed = TRUE
  )

  expect_error(
    recordLinkage(x, y, vars = c("a", "b"), distance = "manhattan"),
    "Use `distance = 'gower'` for mixed-type data.",
    fixed = TRUE
  )
})

test_that("euclidean and manhattan work with numeric, integer, and logical variables", {
  x <- data.frame(
    id = 1:3,
    a = c(0, 1, 2),
    b = c(TRUE, FALSE, TRUE)
  )
  y <- data.frame(
    id = 1:3,
    a = c(0, 1, 2),
    b = c(TRUE, FALSE, TRUE)
  )

  out_euc <- recordLinkage(
    x, y,
    vars = c("a", "b"),
    distance = "euclidean",
    x_id = "id",
    y_id = "id"
  )

  out_man <- recordLinkage(
    x, y,
    vars = c("a", "b"),
    distance = "manhattan",
    x_id = "id",
    y_id = "id"
  )

  expect_equal(out_euc$correct_matches, 3)
  expect_equal(out_man$correct_matches, 3)
  expect_true(all(out_euc$matches$correct_match))
  expect_true(all(out_man$matches$correct_match))
})

test_that("n_best counts row-wise tied minima from the original cost matrix", {
  x <- data.frame(id = 1:2, a = c(0, 1))
  y <- data.frame(id = 1:2, a = c(0, 0))

  out <- recordLinkage(
    x, y,
    vars = "a",
    distance = "euclidean",
    x_id = "id",
    y_id = "id",
    tol = 0
  )

  expect_equal(out$matches$n_best, c(2L, 2L))
})

test_that("zero tolerance means only exact ties are counted in n_best", {
  x <- data.frame(id = 1:2, a = c(0, 2))
  y <- data.frame(id = 1:2, a = c(0, 1e-7))

  out <- recordLinkage(
    x, y,
    vars = "a",
    distance = "euclidean",
    x_id = "id",
    y_id = "id",
    tol = 0
  )

  expect_equal(out$matches$n_best[1], 1L)
})

test_that("unordered factors and characters are harmonized to common nominal levels", {
  x <- data.frame(
    v = factor(c("a", "b", NA), levels = c("a", "b"))
  )
  
  y <- data.frame(
    v = c("b", "c", NA),
    stringsAsFactors = FALSE
  )
  
  out <- .harmonize_linkage_data(x, y)
  
  expect_true(is.factor(out$x$v))
  expect_true(is.factor(out$y$v))
  expect_false(is.ordered(out$x$v))
  expect_false(is.ordered(out$y$v))
  
  expect_identical(levels(out$x$v), c("a", "b", "c"))
  expect_identical(levels(out$y$v), c("a", "b", "c"))
  
  expect_true(is.na(out$x$v[3]))
  expect_true(is.na(out$y$v[3]))
})

test_that("ordered factors with identical levels are preserved as ordered", {
  lev <- c("low", "medium", "high")
  
  x <- data.frame(
    v = ordered(c("low", "high", NA), levels = lev)
  )
  
  y <- data.frame(
    v = ordered(c("medium", "high", NA), levels = lev)
  )
  
  out <- .harmonize_linkage_data(x, y)
  
  expect_true(is.ordered(out$x$v))
  expect_true(is.ordered(out$y$v))
  expect_identical(levels(out$x$v), lev)
  expect_identical(levels(out$y$v), lev)
  
  expect_true(is.na(out$x$v[3]))
  expect_true(is.na(out$y$v[3]))
})


test_that("ordered factors with different level orders throw an error", {
  x <- data.frame(
    v = ordered(c("low", "high"), levels = c("low", "medium", "high"))
  )
  
  y <- data.frame(
    v = ordered(c("medium", "high"), levels = c("medium", "low", "high"))
  )
  
  expect_error(
    .harmonize_linkage_data(x, y),
    "Ordered factor levels for variable `v` are not identical in `x` and `y`."
  )
})

test_that("ordered variable in x and character variable in y are harmonized using x levels", {
  x <- data.frame(
    v = ordered(c("low", "high", NA), levels = c("low", "medium", "high"))
  )
  
  y <- data.frame(
    v = c("medium", "high", NA),
    stringsAsFactors = FALSE
  )
  
  out <- .harmonize_linkage_data(x, y)
  
  expect_true(is.ordered(out$x$v))
  expect_true(is.ordered(out$y$v))
  expect_identical(levels(out$x$v), c("low", "medium", "high"))
  expect_identical(levels(out$y$v), c("low", "medium", "high"))
})

test_that("ordered variable and incompatible character values throw an error", {
  x <- data.frame(
    v = ordered(c("low", "high"), levels = c("low", "medium", "high"))
  )
  
  y <- data.frame(
    v = c("medium", "very_high"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    .harmonize_linkage_data(x, y),
    "Variable `v` is ordered in `x`, but `y` contains values not present in the ordered levels: very_high"
  )
})

test_that("logical variables are preserved as logical", {
  x <- data.frame(v = c(TRUE, FALSE, NA))
  y <- data.frame(v = c(FALSE, TRUE, NA))
  
  out <- .harmonize_linkage_data(x, y)
  
  expect_true(is.logical(out$x$v))
  expect_true(is.logical(out$y$v))
  expect_true(is.na(out$x$v[3]))
  expect_true(is.na(out$y$v[3]))
})

test_that("numeric and integer variables are harmonized to numeric", {
  x <- data.frame(v = c(1L, 2L, NA))
  y <- data.frame(v = c(1.5, 2.5, NA))
  
  out <- .harmonize_linkage_data(x, y)
  
  expect_true(is.numeric(out$x$v))
  expect_true(is.numeric(out$y$v))
  expect_equal(out$x$v[1], 1)
  expect_equal(out$y$v[1], 1.5)
})

test_that("unsupported variable class combinations throw an error", {
  x <- data.frame(v = I(list(1, 2)))
  y <- data.frame(v = I(list(1, 2)))
  
  expect_error(
    .harmonize_linkage_data(x, y),
    "Unsupported or incompatible variable classes for variable `v`"
  )
})

test_that("print.recordLinkage prints key summary fields invisibly", {
  x <- data.frame(id = 1:2, a = c(1, 2))
  y <- data.frame(id = 1:2, a = c(1, 2))

  out <- recordLinkage(
    x, y,
    vars = "a",
    distance = "euclidean",
    x_id = "id",
    y_id = "id"
  )

  expect_output(print(out), "Correct matches:", fixed = TRUE)
  expect_output(print(out), "Correct match percent:", fixed = TRUE)
  expect_output(print(out), "Mean distance:", fixed = TRUE)
  expect_invisible(print(out))
})