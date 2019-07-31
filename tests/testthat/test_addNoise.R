{#Testing on data frame values

  {#data frame, numeric values without missing value

    #load library
    library(testthat)

    set.seed(3)
    sample_Data <- runif(15, 1, 100)
    sample_Data <- array(sample_Data, dim = c(5,3))


    ###########################################################################################################################################
    test_that("data frame, numeric values (default noise, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data)
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-3548.60, -1003.56, 998.53, -4237.93, 786.45, 161.57, 298.97, 3764.03, -4017.58, 4301.45, -1649.26, -2532.50, -1582.16, 633.18, 434.17))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = 20, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20)
      expect_equal(as.vector(round(fr$xm, digits = 2)), c( -457.86,  -63.66,  167.03, -536.07,  157.39,   74.27,   51.42,  528.01, -485.25, 628.53, -175.10, -293.47, -164.27,  133.10,  133.22))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = default, correlated method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-6.57,  76.91,  61.72, -13.83,  81.00,  86.92,  25.28,  40.16,  70.03,  73.90,  51.16,  48.50,  70.58,  37.55, 110.05))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = 20, correlated method)", {  
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(7.35, 77.28, 45.98, 16.67, 68.35, 68.52, 14.90, 32.04, 63.13, 67.65, 53.73, 53.48, 62.12, 48.61, 94.91))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = default, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated2")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(15.40, 80.05, 39.79, 30.66, 61.02, 60.82, 13.69, 32.73, 55.40, 66.20, 50.60, 49.32, 52.81, 56.57, 87.02))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = 20, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated2")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(15.40, 80.05, 39.79, 30.66, 61.02, 60.82, 13.69, 32.73, 55.40, 66.20, 50.60, 49.32, 52.81, 56.57, 87.02))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = default, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "restr")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(34.63, 59.79, 51.43, 41.08, 51.49, 60.30, 32.87, 39.06, 59.22, 53.34, 47.85, 56.28, 49.42, 49.68, 70.95))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = 20, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "restr")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(25.49, 71.17, 44.80, 36.97, 56.39, 60.59, 22.37, 34.28, 58.66, 58.78, 49.91, 53.44, 51.81, 53.17, 79.54))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = default, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "ROMM")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(17.68, 80.86, 39.19, 33.32, 60.73, 60.89, 13.28, 30.17, 58.09, 63.51, 51.74, 50.88, 53.91, 56.04, 87.02))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = 20, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "ROMM")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(17.68, 80.86, 39.19, 33.32, 60.73, 60.89, 13.28, 30.17, 58.09, 63.51, 51.74, 50.88, 53.91, 56.04, 87.02))
    })
    ###########################################################################################################################################



    set.seed(3)
    sample_Data <- runif(30, 1, 100)
    sample_Data <- array(sample_Data, dim = c(10,3))

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = default, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "outdect")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(17.64, 61.09, 39.11, 33.45, 60.61, 60.84, 7.30,  35.51, 34.41, 67.51, 51.69, 51.70, 53.87, 56.17, 86.92, 83.14, 14.04, 96.84, 61.28, 58.41, 23.59, -19.95, 13.77, 10.24, 24.45, 79.32, 26.25, 69.50, 64.10, 80.40))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values (noise = 20, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "outdect")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(17.64, 78.30, 39.11, 33.45, 60.61, 60.84, 12.53, 30.88, 55.01, 64.01, 51.69, 51.09, 53.87, 56.17, 86.92, 83.14, 12.30, 74.16, 86.04, 32.66, 23.59, -0.48, 13.77, 10.24, 24.45, 79.32, 55.82, 88.22, 57.50, 76.43))
    })
    ###########################################################################################################################################
  }









  {#data frame, numeric values with missing value
    set.seed(3)
    sample_Data <- runif(15, 1, 100)
    set.seed(3)
    sample_Data[ceiling(runif(8, 1, length(sample_Data)))] <- NA
    sample_Data <- array(sample_Data, dim = c(5,3))

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (default noise, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data)
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-4646.02, -1337.28, NA, NA, 1009.80, NA, NA, 3348.45, -3563.95, NA, -1863.16, -2857.39, NA, 705.74, 477.84))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (noise = 20, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20)
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-604.18, -108.15, NA, NA, 187.17, NA, NA, 472.60, -424.77, NA, -203.62, -336.79, NA, 142.78, 139.05))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (noise = default, correlated method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-6.57,  76.91,  61.72, -13.83,  81.00,  86.92,  25.28,  40.16,  70.03,  73.90,  51.16,  48.50,  70.58,  37.55, 110.05))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (noise = 20, correlated method)", {  
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(7.35, 77.28, 45.98, 16.67, 68.35, 68.52, 14.90, 32.04, 63.13, 67.65, 53.73, 53.48, 62.12, 48.61, 94.91))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (noise = default, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated2")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(14.70, 79.86, NA, NA, 61.20, NA, NA, 32.45, 55.70, NA, 50.46, 49.11, NA, 56.63, 87.06))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (noise = 20, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated2")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(14.70, 79.86, NA, NA, 61.20, NA, NA, 32.45, 55.70, NA, 50.46, 49.11, NA, 56.63, 87.06))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (noise = default, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "restr")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(38.60, 59.19, NA, NA, 50.88, NA, NA, 38.46, 60.11, NA, 47.24, 57.18, NA, 49.07, 71.85))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (noise = 20, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "restr")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(27.33, 70.89, NA, NA, 56.11, NA, NA, 34.00, 59.08, NA, 49.63, 53.85, NA, 52.89, 79.95))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (noise = default, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(fr$xm, digits = 2)))), TRUE)
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (noise = 20, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(fr$xm, digits = 2)))), TRUE)
    })
    ###########################################################################################################################################



    set.seed(3)
    sample_Data <- runif(30, 1, 100)
    set.seed(3)
    sample_Data[ceiling(runif(4, 1, length(sample_Data)))] <- NA
    sample_Data <- array(sample_Data, dim = c(10,3))

    ###########################################################################################################################################
    test_that("data frame, numeric values with some NAs (noise = default, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "outdect")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(17.64, 60.50, 39.11, 33.45, 60.61, NA, 13.34, 23.95, 63.68, 63.47, NA, 20.59, NA, 56.17, 86.92, 83.14, 12.03, 75.83, 90.65, 28.69, 23.59, 5.19, 13.77, 10.24, NA, 79.32, 60.37, 126.04, 18.35, 75.81))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame method, numeric values with some NAs (noise = 20, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "outdect")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(17.64, 78.22, 39.11, 33.45, 60.61, NA, 13.34, 29.34, 58.92, 63.47, NA, 46.94, NA, 56.17, 86.92, 83.14, 12.03, 71.35, 89.96, 28.69, 23.59, 2.87, 13.77, 10.24, NA, 79.32, 60.37, 95.76, 51.40, 75.81))
    })
    ###########################################################################################################################################
  }









  {#data frame, numeric values with one entire column, one entire row with NA

    set.seed(3)
    sample_Data <- runif(15, 1, 100)
    sample_Data <- array(sample_Data, dim = c(5,3))
    set.seed(3)
    sample_Data[,2] <- NA
    sample_Data[3,] <- NA

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (default noise, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data)
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-4044.73, -1154.43, NA, -4832.15, 887.42, NA, NA, NA, NA, NA, 129.14, 270.61, NA, -3077.54, 3345.35))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = 20, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20)
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-524.01, -83.77, NA, -615.30, 170.85, NA, NA, NA, NA, NA, 62.02, 80.28, NA, -361.66, 521.38))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = default, correlated method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-6.57,  76.91,  61.72, -13.83,  81.00,  86.92,  25.28,  40.16,  70.03,  73.90,  51.16,  48.50,  70.58,  37.55, 110.05))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = 20, correlated method)", {  
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(7.35, 77.28, 45.98, 16.67, 68.35, 68.52, 14.90, 32.04, 63.13, 67.65, 53.73, 53.48, 62.12, 48.61, 94.91))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = default, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated2")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(15.08, 79.96, NA, 30.28, 61.10, NA, NA, NA, NA, NA, 51.79, 51.20, NA, 54.11, 88.97))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = 20, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated2")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(15.08, 79.96, NA, 30.28, 61.10, NA, NA, NA, NA, NA, 51.79, 51.20, NA, 54.11, 88.97))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = default, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "restr")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(35.70, NaN, NA, 42.15, NaN, NA, NA, NA, NA, NA, NaN, 57.18, NA, NaN, 71.85))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = 20, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "restr")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(25.98, NaN, NA, 37.47, NaN, NA, NA, NA, NA, NA, NaN, 53.85, NA, NaN, 79.95))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = default, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(fr$xm, digits = 2)))), TRUE)
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = 20, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(fr$xm, digits = 2)))), TRUE)
    })
    ###########################################################################################################################################


    set.seed(3)
    sample_Data <- runif(30, 1, 100)
    sample_Data <- array(sample_Data, dim = c(10,3))
    set.seed(3)
    sample_Data[,2] <- NA
    sample_Data[3,] <- NA

    ###########################################################################################################################################
    test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = default, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "outdect")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(17.64, 61.09, 39.11, 33.45, 60.61, 60.84, 7.30,  35.51, 34.41, 67.51, 51.69, 51.70, 53.87, 56.17, 86.92, 83.14, 14.04, 96.84, 61.28, 58.41, 23.59, -19.95, 13.77, 10.24, 24.45, 79.32, 26.25, 69.50, 64.10, 80.40))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame method, numeric values with one entire column, one entire row with NAs (noise = 20, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "outdect")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(17.64, 78.30, 39.11, 33.45, 60.61, 60.84, 12.53, 30.88, 55.01, 64.01, 51.69, 51.09, 53.87, 56.17, 86.92, 83.14, 12.30, 74.16, 86.04, 32.66, 23.59, -0.48, 13.77, 10.24, 24.45, 79.32, 55.82, 88.22, 57.50, 76.43))
    })
    ###########################################################################################################################################
  }









  {#data frame, numeric values with only one column with NAs

    set.seed(3)
    sample_Data <- runif(5, 1, 100)
    sample_Data[c(2,4)] <- NA
    sample_Data <- as.data.frame(sample_Data)

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (default noise, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data)
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-3082.57, NA, 873.16, NA, 691.59))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (noise = 20, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20)
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-395.72, NA, 150.32, NA, 144.74))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (noise = default, correlated method)", {
      set.seed(3)
      expect_error(addNoise(sample_Data, method = "correlated"), regexp = "must have more than 2 variables")
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (noise = 20, correlated method)", {  
      set.seed(3)
      expect_error(addNoise(sample_Data, method = "correlated", noise = 20), regexp = "must have more than 2 variables")
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (noise = default, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated2")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(15.68, NA, 39.67, NA, 60.92))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (noise = 20, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated2")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(15.68, NA, 39.67, NA, 60.92))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (noise = default, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "restr")

      #this method returns fr$xm in a different form
      expect_equal(as.vector(round(as.matrix(fr$xm), digits = 2)), c(30.35, NA, 39.11, NA, 47.89))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (noise = 20, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "restr")

      #this method returns fr$xm in a different form
      expect_equal(as.vector(round(as.matrix(fr$xm), digits=2)), c(23.51, NA, 39.11, NA, 54.73))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (noise = default, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(fr$xm, digits = 2)))), TRUE)
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (noise = 20, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(fr$xm, digits = 2)))), TRUE)
    })
    ###########################################################################################################################################



    set.seed(3)
    sample_Data <- runif(10, 1, 100)
    sample_Data[c(2,4,5,7)] <- NA
    sample_Data <- as.data.frame(sample_Data)

    ###########################################################################################################################################
    test_that("data frame, numeric values with only one column with NAs (noise = default, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "outdect")
      expect_equal(as.vector(round(unlist(fr$xm), digits = 2)), c(17.64, NA, 39.11, NA, NA, 60.84, NA, 30.17, 58.18, 46.64))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame method, numeric values with only one column with NAs (noise = 20, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "outdect")
      expect_equal(as.vector(round(unlist(fr$xm), digits = 2)), c(17.64, NA, 39.11, NA, NA, 60.84, NA, 30.17, 58.18, 61.22))
    })
    ###########################################################################################################################################
  }









  {#data frame, numeric values with more columns then rows

    set.seed(3)
    sample_Data <- runif(15, 1, 100)
    sample_Data <- array(sample_Data, dim = c(3,5))

    ###########################################################################################################################################
    test_that("data frame, numeric values with more columns then rows (default noise, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data)
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-4627.80, -1331.74, 1288.87, -2688.14, 523.09, 131.99, 303.60, 3824.50, -4083.59, 1395.73, -731.23, -1138.15, -1929.34, 755.63, 507.86))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with more columns then rows (noise = 20, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20)
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-601.76, -107.41, 205.74, -329.43, 122.27, 70.32, 52.04, 536.08, -494.05, 241.10, -52.70, -107.56, -210.56, 149.43, 143.05))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with more columns then rows (noise = default, correlated method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-37.30, 74.45, 48.66, 20.20, 54.28, 65.58, 20.96, 20.81, 62.12, 68.87, 54.55, 48.92, 70.20, 49.64, 88.42))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with more columns then rows (noise = 20, correlated method)", {  
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(-2.42, 78.57, 42.60, 28.61, 58.30, 62.57, 16.12, 26.75, 59.62, 65.44, 52.73, 50.24, 59.83, 53.78, 87.47))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with more columns then rows (noise = default, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated2")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(14.68, 79.83, 39.98, 31.72, 60.87, 60.84, 13.64, 32.71, 55.30, 64.31, 51.19, 50.23, 52.61, 56.68, 87.10))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with more columns then rows (noise = 20, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated2")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(14.68, 79.83, 39.98, 31.72, 60.87, 60.84, 13.64, 32.71, 55.30, 64.31, 51.19, 50.23, 52.61, 56.68, 87.10))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with more columns then rows (noise = default, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "restr")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(39.58, 58.18, 35.06, 50.48, 64.53, 49.24, 43.07, 33.06, 56.01, 65.16, 47.19, 51.49, 38.36, 55.56, 70.41))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with more columns then rows (noise = 20, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "restr")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(28.59, 69.58, 37.09, 41.95, 62.56, 55.04, 28.18, 31.61, 57.10, 64.31, 49.44, 51.24, 46.13, 55.86, 78.68))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with more columns then rows (noise = default, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "ROMM")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(17.65, 80.94, 39.12, 33.45, 60.60, 60.85, 13.33, 30.16, 58.19, 63.47, 51.67, 51.02, 53.86, 56.15, 86.94))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("data frame, numeric values with more columns then rows (noise = 20, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "ROMM")
      expect_equal(as.vector(round(fr$xm, digits = 2)), c(17.65, 80.94, 39.12, 33.45, 60.60, 60.85, 13.33, 30.16, 58.19, 63.47, 51.67, 51.02, 53.86, 56.15, 86.94))
    })
    ###########################################################################################################################################
    








  {#data frame, numeric values with factors

    set.seed(3)
    sample_Data <- runif(15, 1, 3)
    sample_Data <- as.factor(round(array(sample_Data, dim = c(15,1)), digits = 0))

    ##########################################################################################################
    test_that("data frame, numeric with factors", { 
      set.seed(3)
      expect_error(addNoise(sample_Data), regexp = "unable to find an inherited method for function ‘addNoise’ for signature .*[factor].*")
    })
    ##########################################################################################################
  }
}

















































{#Testing on sdc  values

  {#sdc object, numeric values without missing value

    #load library
    library(testthat)

    set.seed(3)
    sample_Data <- runif(15, 1, 100)
    sample_Data <- array(sample_Data, dim = c(5,3))
    colnames(sample_Data) <- c("Var1", "Var2", "Var3")
    sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1", "Var2", "Var3"), numVars = c("Var1", "Var2", "Var3"))


    ###########################################################################################################################################
    test_that("sdc object, numeric values (default noise, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-3548.60, -1003.56, 998.53, -4237.93, 786.45, 161.57, 298.97, 3764.03, -4017.58, 4301.45, -1649.26, -2532.50, -1582.16, 633.18, 434.17))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = 20, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c( -457.86,  -63.66,  167.03, -536.07,  157.39,   74.27,   51.42,  528.01, -485.25, 628.53, -175.10, -293.47, -164.27,  133.10,  133.22))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = default, correlated method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-6.57,  76.91,  61.72, -13.83,  81.00,  86.92,  25.28,  40.16,  70.03,  73.90,  51.16,  48.50,  70.58,  37.55, 110.05))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = 20, correlated method)", {  
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(7.35, 77.28, 45.98, 16.67, 68.35, 68.52, 14.90, 32.04, 63.13, 67.65, 53.73, 53.48, 62.12, 48.61, 94.91))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = default, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated2")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(15.40, 80.05, 39.79, 30.66, 61.02, 60.82, 13.69, 32.73, 55.40, 66.20, 50.60, 49.32, 52.81, 56.57, 87.02))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = 20, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated2")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(15.40, 80.05, 39.79, 30.66, 61.02, 60.82, 13.69, 32.73, 55.40, 66.20, 50.60, 49.32, 52.81, 56.57, 87.02))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = default, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "restr")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(34.63, 59.79, 51.43, 41.08, 51.49, 60.30, 32.87, 39.06, 59.22, 53.34, 47.85, 56.28, 49.42, 49.68, 70.95))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = 20, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "restr")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(25.49, 71.17, 44.80, 36.97, 56.39, 60.59, 22.37, 34.28, 58.66, 58.78, 49.91, 53.44, 51.81, 53.17, 79.54))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = default, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "ROMM")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(17.68, 80.86, 39.19, 33.32, 60.73, 60.89, 13.28, 30.17, 58.09, 63.51, 51.74, 50.88, 53.91, 56.04, 87.02))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = 20, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "ROMM")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(17.68, 80.86, 39.19, 33.32, 60.73, 60.89, 13.28, 30.17, 58.09, 63.51, 51.74, 50.88, 53.91, 56.04, 87.02))
    })
    ###########################################################################################################################################



    set.seed(3)
    sample_Data <- runif(30, 1, 100)
    sample_Data <- array(sample_Data, dim = c(10,3))
    colnames(sample_Data) <- c("Var1", "Var2", "Var3")
    sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1", "Var2", "Var3"), numVars = c("Var1", "Var2", "Var3"))

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = default, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "outdect")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(17.64, 61.09, 39.11, 33.45, 60.61, 60.84, 7.30,  35.51, 34.41, 67.51, 51.69, 51.70, 53.87, 56.17, 86.92, 83.14, 14.04, 96.84, 61.28, 58.41, 23.59, -19.95, 13.77, 10.24, 24.45, 79.32, 26.25, 69.50, 64.10, 80.40))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values (noise = 20, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "outdect")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(17.64, 78.30, 39.11, 33.45, 60.61, 60.84, 12.53, 30.88, 55.01, 64.01, 51.69, 51.09, 53.87, 56.17, 86.92, 83.14, 12.30, 74.16, 86.04, 32.66, 23.59, -0.48, 13.77, 10.24, 24.45, 79.32, 55.82, 88.22, 57.50, 76.43))
    })
    ###########################################################################################################################################s
  }









  {#sdc object, numeric values with missing value
    set.seed(3)
    sample_Data <- runif(15, 1, 100)
    set.seed(3)
    sample_Data[ceiling(runif(4, 1, length(sample_Data)))] <- NA
    sample_Data <- array(sample_Data, dim = c(5,3))
    colnames(sample_Data) <- c("Var1", "Var2", "Var3")
    sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1", "Var2", "Var3"), numVars = c("Var1", "Var2", "Var3"))


    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (default noise, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-3921.04, -1116.82, 1098.73, NA, 862.25, NA, NA, 3027.83, -3213.98, 3465.86, -1863.16, -2857.39, NA, 705.74, 477.84))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (noise = 20, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-507.52, -78.76, 180.39, NA, 167.49, NA, NA, 429.85, -378.10, 517.12, -203.62, -336.79, NA, 142.78, 139.05))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (noise = default, correlated method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated")
      sexpect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-6.57,  76.91,  61.72, -13.83,  81.00,  86.92,  25.28,  40.16,  70.03,  73.90,  51.16,  48.50,  70.58,  37.55, 110.05))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (noise = 20, correlated method)", {  
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(7.35, 77.28, 45.98, 16.67, 68.35, 68.52, 14.90, 32.04, 63.13, 67.65, 53.73, 53.48, 62.12, 48.61, 94.91))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (noise = default, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated2")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(15.17, 79.99, 39.87, NA, 61.09, NA, NA, 32.27, 55.96, 65.67, 50.46, 49.11, NA, 56.63, 87.06))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (noise = 20, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated2")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(15.17, 79.99, 39.87, NA, 61.09, NA, NA, 32.27, 55.96, 65.67, 50.46, 49.11, NA, 56.63, 87.06))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (noise = default, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "restr")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(36.54, 62.99, 52.33, NA, 54.69, NA, NA, 42.26, 60.11, 55.25, 51.05, 57.18, NA, 52.88, 71.85))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (noise = 20, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "restr")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(26.37, 72.65, 45.22, NA, 57.87, NA, NA, 35.76, 59.08, 59.67, 51.39, 53.85, NA, 54.65, 79.95))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (noise = default, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)))), TRUE)
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (noise = 20, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)))), TRUE)
    })
    ###########################################################################################################################################



    set.seed(3)
    sample_Data <- runif(30, 1, 100)
    set.seed(3)
    sample_Data[ceiling(runif(2, 1, length(sample_Data)))] <- NA
    sample_Data <- array(sample_Data, dim = c(10,3))
    colnames(sample_Data) <- c("Var1", "Var2", "Var3")
    sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1", "Var2", "Var3"), numVars = c("Var1", "Var2", "Var3"))

    ###########################################################################################################################################
    test_that("sdc object, numeric values with some NAs (noise = default, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "outdect")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(17.64, 60.50, 39.11, 33.45, 60.61, NA, 13.34, 23.95, 63.68, 63.47, 51.69, 23.99, 53.87, 56.17, 86.92, 83.14, 12.03, 75.26, 90.56, 28.69, 23.59, 5.19, 13.77, 10.24, NA, 79.32, 60.37, 126.04, 18.35, 75.81))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object method, numeric values with some NAs (noise = 20, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "outdect")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(17.64, 78.22, 39.11, 33.45, 60.61, NA, 13.34, 29.34, 58.92, 63.47, 51.69, 47.40, 53.87, 56.17, 86.92, 83.14, 12.03, 71.28, 89.95, 28.69, 23.59, 2.87, 13.77, 10.24, NA, 79.32, 60.37, 95.76, 51.40, 75.81))
    })
    ###########################################################################################################################################
  }









  {#sdc object, numeric values with one entire column, one entire row with NA

    set.seed(3)
    sample_Data <- runif(15, 1, 100)
    set.seed(3)
    sample_Data <- array(sample_Data, dim = c(5,3))
    sample_Data[, 2] <- NA
    sample_Data[3, ] <- NA
    colnames(sample_Data) <- c("Var1", "Var2", "Var3")
    sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1", "Var2", "Var3"), numVars = c("Var1", "Var2", "Var3"))


    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (default noise, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-4044.73, -1154.43, NA, -4832.15, 887.42, NA, NA, NA, NA, NA, 129.14, 270.61, NA, -3077.54, 3345.35))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (noise = 20, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-524.01, -83.77, NA, -615.30, 170.85, NA, NA, NA, NA, NA, 62.02, 80.28, NA, -361.66, 521.38))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (noise = default, correlated method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-6.57,  76.91,  61.72, -13.83,  81.00,  86.92,  25.28,  40.16,  70.03,  73.90,  51.16,  48.50,  70.58,  37.55, 110.05))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (noise = 20, correlated method)", {  
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(7.35, 77.28, 45.98, 16.67, 68.35, 68.52, 14.90, 32.04, 63.13, 67.65, 53.73, 53.48, 62.12, 48.61, 94.91))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (noise = default, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated2")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(15.08, 79.96, NA, 30.28, 61.10, NA, NA, NA, NA, NA, 51.79, 51.20, NA, 54.11, 88.97))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (noise = 20, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated2")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(15.08, 79.96, NA, 30.28, 61.10, NA, NA, NA, NA, NA, 51.79, 51.20, NA, 54.11, 88.97))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (noise = default, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "restr")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(35.70, NaN, NA, 42.15, NaN, NA, NA, NA, NA, NA, NaN, 57.18, NA, NaN, 71.85))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (noise = 20, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "restr")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(25.98, NaN, NA, 37.47, NaN, NA, NA, NA, NA, NA, NaN, 53.85, NA, NaN, 79.95))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (noise = default, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)))), TRUE)
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (noise = 20, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)))), TRUE)
    })
    ###########################################################################################################################################


    set.seed(3)
    sample_Data <- runif(30, 1, 100)
    sample_Data <- array(sample_Data, dim = c(10,3))
    set.seed(3)
    sample_Data[,2] <- NA
    sample_Data[3,] <- NA

    ###########################################################################################################################################
    test_that("sdc object, numeric values with one entire column, one entire row with NAs (noise = default, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "outdect")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(17.64, 61.09, 39.11, 33.45, 60.61, 60.84, 7.30,  35.51, 34.41, 67.51, 51.69, 51.70, 53.87, 56.17, 86.92, 83.14, 14.04, 96.84, 61.28, 58.41, 23.59, -19.95, 13.77, 10.24, 24.45, 79.32, 26.25, 69.50, 64.10, 80.40))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object method, numeric values with one entire column, one entire row with NAs (noise = 20, outdect method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "outdect")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(17.64, 78.30, 39.11, 33.45, 60.61, 60.84, 12.53, 30.88, 55.01, 64.01, 51.69, 51.09, 53.87, 56.17, 86.92, 83.14, 12.30, 74.16, 86.04, 32.66, 23.59, -0.48, 13.77, 10.24, 24.45, 79.32, 55.82, 88.22, 57.50, 76.43))
    })
    ###########################################################################################################################################
  }









  {#sdc object, numeric values with only one column with NAs

    set.seed(3)
    sample_Data <- runif(10, 1, 100)
    set.seed(5)
    sample_Data[ceiling(runif(4, 1, length(sample_Data)))] <- NA
    sample_Data <- array(sample_Data, dim = c(10,1))
    colnames(sample_Data) <- c("Var1")
    sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1"), numVars = c("Var1"))

    ###########################################################################################################################################
    test_that("sdc object, numeric values with only one column with NAs (default noise, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-3872.31, -1101.99, NA, NA, 852.33, 182.65, 358.76, NA, -4870.73, NA))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with only one column with NAs (noise = 20, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-501.02, -76.78, NA, NA, 166.17, 77.08, 59.39, NA, -599.00, NA))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with only one column with NAs (noise = default, correlated method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated", noise = 150)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-14.13, 71.29, NA, NA, 67.07, 61.83, 16.16, NA, 17.94, NA))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with only one column with NAs (noise = 20, correlated method)", {  
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated", noise = 20)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(6.04, 77.42, NA, NA, 62.97, 61.20, 14.37, NA, 43.49, NA))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with only one column with NAs (noise = default, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated2")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(15.20, 79.99, NA, NA, 61.08, 60.85, 13.75, NA, 54.85, NA))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with only one column with NAs (noise = 20, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated2")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(15.20, 79.99, NA, NA, 61.08, 60.85, 13.75, NA, 54.85, NA))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with only one column with NAs (noise = default, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "restr")

      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(9.21, 65.49, NA, NA, 31.65, 54.99, 6.97, NA, 30.39, NA))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with only one column with NAs (noise = 20, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "restr")

      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(14.40, 75.01, NA, NA, 49.49, 58.59, 10.89, NA, 47.51, NA))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with only one column with NAs (noise = default, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)))), TRUE)
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with only one column with NAs (noise = 20, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "ROMM")
      expect_equal(all(is.na(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)))), TRUE)
    })
    ###########################################################################################################################################
  }









  {#sdc object, numeric values with more columns then rows

    set.seed(3)
    sample_Data <- runif(15, 1, 100)
    sample_Data <- array(sample_Data, dim = c(3,5))
    colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5")
    sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1", "Var2", "Var3", "Var4", "Var5"), numVars = c("Var1", "Var2", "Var3", "Var4", "Var5"))


    ###########################################################################################################################################
    test_that("sdc object, numeric values with more columns then rows (default noise, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-4627.80, -1331.74, 1288.87, -2688.14, 523.09, 131.99, 303.60, 3824.50, -4083.59, 1395.73, -731.23, -1138.15, -1929.34, 755.63, 507.86))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with more columns then rows (noise = 20, additive method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20)
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-601.76, -107.41, 205.74, -329.43, 122.27, 70.32, 52.04, 536.08, -494.05, 241.10, -52.70, -107.56, -210.56, 149.43, 143.05))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with more columns then rows (noise = default, correlated method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-37.30, 74.45, 48.66, 20.20, 54.28, 65.58, 20.96, 20.81, 62.12, 68.87, 54.55, 48.92, 70.20, 49.64, 88.42))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with more columns then rows (noise = 20, correlated method)", {  
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(-2.42, 78.57, 42.60, 28.61, 58.30, 62.57, 16.12, 26.75, 59.62, 65.44, 52.73, 50.24, 59.83, 53.78, 87.47))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with more columns then rows (noise = default, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "correlated2")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(14.68, 79.83, 39.98, 31.72, 60.87, 60.84, 13.64, 32.71, 55.30, 64.31, 51.19, 50.23, 52.61, 56.68, 87.10))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with more columns then rows (noise = 20, correlated2 method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "correlated2")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(14.68, 79.83, 39.98, 31.72, 60.87, 60.84, 13.64, 32.71, 55.30, 64.31, 51.19, 50.23, 52.61, 56.68, 87.10))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with more columns then rows (noise = default, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "restr")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(39.58, 58.18, 35.06, 50.48, 64.53, 49.24, 43.07, 33.06, 56.01, 65.16, 47.19, 51.49, 38.36, 55.56, 70.41))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with more columns then rows (noise = 20, restr method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "restr")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(28.59, 69.58, 37.09, 41.95, 62.56, 55.04, 28.18, 31.61, 57.10, 64.31, 49.44, 51.24, 46.13, 55.86, 78.68))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with more columns then rows (noise = default, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, method = "ROMM")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(17.65, 80.94, 39.12, 33.45, 60.60, 60.85, 13.33, 30.16, 58.19, 63.47, 51.67, 51.02, 53.86, 56.15, 86.94))
    })
    ###########################################################################################################################################

    ###########################################################################################################################################
    test_that("sdc object, numeric values with more columns then rows (noise = 20, ROMM method)", {
      set.seed(3)
      fr <- addNoise(sample_Data, noise = 20, method = "ROMM")
      expect_equal(as.vector(round(unlist(unclass(fr@manipNumVars)), digits = 2)), c(17.65, 80.94, 39.12, 33.45, 60.60, 60.85, 13.33, 30.16, 58.19, 63.47, 51.67, 51.02, 53.86, 56.15, 86.94))
    })
    ###########################################################################################################################################
  }
    








  {#sdc object, numeric values with factors

    set.seed(3)
    sample_Data <- runif(15, 1, 3)
    sample_Data <- round(array(sample_Data, dim = c(5,3)), digits = 0)
    sample_Data <- apply(sample_Data, 2, as.factor)
    colnames(sample_Data) <- c("Var1", "Var2", "Var3")
    sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1", "Var2", "Var3"))


    ##########################################################################################################
    test_that("sdc object, numeric with factors", { 
      set.seed(3)
      expect_error(addNoise(sample_Data), regexp = "argument of length 0")
    })
    ##########################################################################################################
  }
}
