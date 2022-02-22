#################################
# test recordSwap with sdcObject
#

context("test recordSwap() with sdcObject")
library(sdcMicro)

seed <- 2021
set.seed(seed)
nhid <- 5000
dat <- sdcMicro::createDat( nhid )

k_anonymity <- 0
swaprate <- .05
similar <- list(c("hsize"))
hier <- c("nuts1","nuts2","nuts3")
risk_variables <- c("ageGroup","national")
hid <- "hid"

data_sdc <- createSdcObj(dat,hhId = hid,
                         keyVars=risk_variables,
                         seed = seed,options = list(k_anonymity = k_anonymity,
                                                    swaprate = swaprate,
                                                    similar = similar,
                                                    hierarchy = hier))
# test input parameter
test_that("test input for sdcObject",{
  
  #################################
  # normal input
  expect_error(recordSwap(data = data_sdc),NA)
  
  # params missing
  data_sdc@options$hierarchy <- NULL
  expect_error(recordSwap(data = data_sdc),"argument `hierarchy` is missing, with no default\n Alternatively one can specifcy `hierarchy` through the parameter `options` in `createSdcObj\\(\\)`")
  data_sdc@options$hierarchy <- hier
  
  hidnum <- copy(data_sdc@hhId)
  data_sdc@hhId <- NULL
  expect_error(recordSwap(data = data_sdc),"argument `hid` is missing, with no default\n Alternatively one can specifcy `hid` through the parameter `hhId` in `createSdcObj\\(\\)`")
  data_sdc@hhId <- hidnum
  
  # extra params in function input
  expect_warning(recordSwap(data = data_sdc,risk_variables=risk_variables),"argument `risk_variables` defined in function call and in `data`: taking value from function call")
  
  # check that results with both methods are idenical
  data_sw_sdc <- recordSwap(data = data_sdc,return_swapped_id=TRUE)
  data_sw_norm <- recordSwap(data = dat, hid = hid, hierarchy = hier,
                             similar = similar, swaprate = swaprate,
                             k_anonymity = k_anonymity,
                             risk_variables = risk_variables,
                             carry_along = NULL,
                             return_swapped_id = TRUE,
                             seed=seed)
  
  expect_true(all.equal(data_sw_norm,data_sw_sdc))
})
