context("test ldiversity()")
library(sdcMicro)

test_that("ldiversityWORK handles NAs and distinct counts correctly", {
  # 1. Setup the specific data frame that previously failed
  test_data <- data.frame(
    sex = c("female", "female", "female", "male", "male", "female", "female"),
    occupation = c(NA, NA, NA, "teacher", "teacher", "nurse", "nurse"),
    ethnicity = c("other", "other", "other", "other", "other", "majority", "majority"),
    sensitive = c(1, 0, 2, 1, 0, 0, 1)
  )

  # 2. Run the function
  # keyVars: sex, occupation, ethnicity (col 1:3)
  # ldiv_index: sensitive (col 4)
  res <- ldiversity(
    obj = test_data,
    keyVars = 1:3,
    ldiv_index = 4,
    missing = -999
  )

  # Convert to matrix/df for easier checking if not already
  res <- as.data.frame(res[])

  # Test Case: Group 1 (Rows 1-3)
  # Values: 1, 0, 2 -> Distinct should be 3
  expect_equal(res$sensitive_Distinct_Ldiversity[1], 3)
  expect_equal(res$sensitive_Distinct_Ldiversity[2], 3)
  expect_equal(res$sensitive_Distinct_Ldiversity[3], 3)

  # Test Case: Group 2 (Rows 4-5)
  # Values: 1, 0 -> Distinct should be 2
  expect_equal(res$sensitive_Distinct_Ldiversity[4], 2)
  expect_equal(res$sensitive_Distinct_Ldiversity[5], 2)

  # Test Case: Group 3 (Rows 6-7)
  # Values: 0, 1 -> Distinct should be 2
  expect_equal(res$sensitive_Distinct_Ldiversity[6], 2)
  expect_equal(res$sensitive_Distinct_Ldiversity[7], 2)
})

test_that("ldiversityWORK maintains original row order", {
  # Create a dataset where the sorted order is different from input order
  unordered_data <- data.frame(
    id = c(3, 1, 2), # If we sort by 'id', order changes
    sens = c(10, 20, 10)
  )

  # Group 1: id=3, sens=10 (Size 1, Distinct 1)
  # Group 2: id=1, sens=20 (Size 1, Distinct 1)
  # Group 3: id=2, sens=10 (Size 1, Distinct 1)
  res <- ldiversity(
    obj = unordered_data,
    keyVars = "id",
    ldiv_index = "sens"
  )
  res <- as.data.frame(res[])

  # The result should have 3 rows and match the input rows
  expect_equal(nrow(res), 3)
  expect_true(all(res[, 1] == 1)) # All groups size 1 in this specific setup
})

test_that("ldiversity handles multiple sensitive variables", {
  multi_data <- data.frame(
    key = c(1, 1, 1),
    sens1 = c("A", "B", "A"), # 2 distinct
    sens2 = c("X", "Y", "Z")  # 3 distinct
  )

  res <- ldiversity(obj = multi_data, keyVars = "key", ldiv_index = c("sens1", "sens2"))
  res <- as.data.frame(res[])
  expect_equal(res[1, "sens1_Distinct_Ldiversity"], 2)
  expect_equal(res[1, "sens2_Distinct_Ldiversity"], 3)
})
