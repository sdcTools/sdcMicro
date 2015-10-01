df <- data.frame("region"=c("A", "A", "A", "A", "A"),
                 "status"=c("single", "married","married","single", "widow"),
                 "agegroup"=c("5","5","5","5","5"))

test_that("data frame method gives the same as method on sdcMicro object", {
  fr <- freqCalc(df, keyVars = 1:3)
  fr <- kAnon(df, keyVars =1:3)
  sdc <- createSdcObj(df, keyVars = 1:3)
  sdc <- kAnon(sdc)
  expect_equal(extractManipData(sdc), fr$xAnon)
})

## four methods will be tested:
## 1: current method --> missing values increase frequencies in other categories
## 2: safe method --> misssing values do not increase frequencies in other categories but in those observation where a missing is set
## 3: c method --> missing values do increase frequencies in other categories by c
## 4: safe method 2 --> misssing values do not increase frequencies in other categories and also not in those observation
## where a missing is set
## 5: same as 4 but missings are treated like an own category
## 6: ? what else?

## the result should look like (method 1) (2 and 3-anonymity)
##   region  status agegroup  freq
## 1      A  single        5     3
## 2      A married        5     3
## 3      A married        5     3
## 4      A  single        5     3
## 5      A       *        5     5


## the result should look like (method 2) (2-anonymity)
##   region  status agegroup  freq
## 1      A  single        5     2
## 2      A married        5     2
## 3      A married        5     2
## 4      A  single        5     2
## 5      A       *        5     5

## the result should look like (method 2) (3-anonymity)
##   region  status agegroup  freq
## 1      A       *        5     5
## 2      A       *        5     5
## 3      A       *        5     5
## 4      A       *        5     5
## 5      A       *        5     5


## the result should look like (method 3) (2-anonymity)
##   region  status agegroup  freq
## 1      A  single        5     2.4
## 2      A married        5     2.4
## 3      A married        5     2.4
## 4      A  single        5     2.4
## 5      A       *        5     5 (or 3.2 (to be discussed))

## the result should look like (method 3) (3-anonymity)
##   region  status agegroup  freq
## 1      A       *        5     5
## 2      A married        5     3.2
## 3      A married        5     3.2
## 4      A       *        5     5
## 5      A       *        5     5


## the result should look like (method 4) (2-anonymity)
##   region  status agegroup  freq
## 1      A       *        5     5
## 2      A married        5     2
## 3      A married        5     2
## 4      A       *        5     5
## 5      A       *        5     5

## the result should look like (method 4) (3-anonymity)
##   region  status agegroup  freq
## 1      A       *        5     5
## 2      A       *        5     5
## 3      A       *        5     5
## 4      A       *        5     5
## 5      A       *        5     5

## the result should look like (method 5) (2-anonymity)
##   region  status agegroup  freq
## 1      A       *        5     3
## 2      A married        5     2
## 3      A married        5     2
## 4      A       *        5     3
## 5      A       *        5     3

## the result should look like (method 5) (3-anonymity)
##   region  status agegroup  freq
## 1      A       *        5     5
## 2      A       *        5     5
## 3      A       *        5     5
## 4      A       *        5     5
## 5      A       *        5     5

## what else (scenario?)

## method1 should give on original data:

test_that("method1 frequencies should be 2,2,2,2,1", {
  sdc <- createSdcObj(df, keyVars = 1:3)
  expect_equal(sdc@risk$individual[,"fk"], c(2,2,2,2,1))
})

## method1 should give to suppressed data:

test_that("method1 frequencies should be 2,2,2,2,1", {
  sdc <- createSdcObj(df, keyVars = 1:3)
  sdc <- kAnon(sdc)
  expect_equal(sdc@risk$individual[,"fk"], c(3,3,3,3,5))
})
