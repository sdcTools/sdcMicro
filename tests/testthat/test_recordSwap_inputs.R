#################################
# test recordSwap
#

context("test recordSwap() inputs")
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

risk <- matrix(
  data = rep(1,3*dim(dat)[1]),
  ncol = 3
)
colnames(risk)  <- c("a","b","c")
risk_threshold <- 0.9

# test input parameter
test_that("test para - data, hid, hierarchy, similar, risk_variables, carry_along",{
  
  #################################
  # data
  dat_wrong <- 1:10
  expect_error(recordSwap(data = dat_wrong, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               "data must be either a data.table, data.frame")
  
  expect_error(recordSwap(data = as.data.frame(dat), hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               NA)
  
  #################################
  
  #################################
  # hid
  expect_error(recordSwap(data = dat, hid = -1, hierarchy = hier,
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = risk_variables,
                            carry_along = NULL,
                            return_swapped_id = TRUE,
                            seed=seed),"Column indices cannot be negative")
  
  expect_error(recordSwap(data = dat, hid = "hhids", hierarchy = hier,
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = risk_variables,
                            carry_along = NULL,
                            return_swapped_id = TRUE,
                            seed=seed),"Column name\\(s\\) in hid are not found in data")
  
  expect_error(recordSwap(data = dat, hid = c("HHID","hhids"), hierarchy = hier,
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = risk_variables,
                            carry_along = NULL,
                            return_swapped_id = TRUE,
                            seed=seed),"hid must be an integer \\(column index\\) or character \\(column name\\) of length 1")
  #################################
  
  #################################
  # hierarchy
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = c(10:100),
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = risk_variables,
                            carry_along = NULL,
                            return_swapped_id = TRUE,
                            seed=seed),"Column index in hierarchy exceeds number of columns in data")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = c("GEM","BDL","GKZ"),
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = risk_variables,
                            carry_along = NULL,
                            return_swapped_id = TRUE,
                            seed=seed),"Column name\\(s\\) in hierarchy are not found in data")
  
  dat[,h_extra:=runif(.N)]
  dat[,h_extra2:=sample(LETTERS,.N,replace=TRUE)]
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = c("h_extra","h_extra2"),
             similar = similar, swaprate = swaprate,
             k_anonymity = k_anonymity,
             risk_variables = risk_variables,
             carry_along = NULL,
             return_swapped_id = TRUE,
             seed=seed),"Columns specified in hid, hierarchy, similar, risk_variables and carry\\_along must contain only integer values at this point")
  dat[,c("h_extra","h_extra2"):=NULL]
  #################################
  
  #################################
  # similarity
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = -1, swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),"Column indices cannot be negative")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = c("hsize","hstatus"), swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),"Column name\\(s\\) in X\\[\\[i\\]\\] are not found in data")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = list(c("hsize","hstatus"),"hsize"), swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),"Column name\\(s\\) in X\\[\\[i\\]\\] are not found in data")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = list(c("hsize","htype"),"hsize"), swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),NA)
  
  #################################
  
  #################################
  # risk_variables
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = c(-1,100),
                            carry_along = NULL,
                            return_swapped_id = TRUE,
                            seed=seed),"Column index in risk\\_variables exceeds number of columns in data")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = c("risk1","risk2"),
                            carry_along = NULL,
                            return_swapped_id = TRUE,
                            seed=seed),"Column name\\(s\\) in risk\\_variables are not found in data")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = data.frame(r=runif(100),r2=1:100),
                            carry_along = NULL,
                            return_swapped_id = TRUE,
                            seed=seed),"risk\\_variables must contain integers \\(column indices\\) or characters \\(\\~column names\\) of data")
  #################################

  #################################
  # carry_along
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = risk_variables,
                            carry_along = -1,
                            return_swapped_id = TRUE,
                            seed=seed),"Column indices cannot be negative")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = risk_variables,
                            carry_along = c("carry_along"),
                            return_swapped_id = TRUE,
                            seed=seed),"Column name\\(s\\) in carry_along are not found in data")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                            similar = similar, swaprate = swaprate,
                            k_anonymity = k_anonymity,
                            risk_variables = risk_variables,
                            carry_along = c("lau2"),
                            return_swapped_id = TRUE,
                            seed=seed),NA)
  
})


test_that("test para - swaprate, k_anonymity, return_swapped_id, seed",{
  
  #################################
  # swaprate
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = "0.1",
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),"swaprate must be a single number between 0 and 1!")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = c(0.1,0.5),
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),"swaprate must be a single number between 0 and 1!")
  
  #################################
  # k_anonymity
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = -1,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),"k_anonymity must be a positiv single integer!")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = c(1,6),
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),"k_anonymity must be a positiv single integer!")
  
  ##################################
  # return_swapped_id
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = c(TRUE,TRUE),
                          seed=seed),"return\\_swapped_id must be logical of length 1")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = "HID",
                          seed=seed),"return\\_swapped_id must be logical of length 1")
  
  ##################################
  # seed
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=1.5),"seed must be a single positive integer!")
  
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=c("a","b")),"seed must be a single positive integer!")
})

# test risk and risk_threshold
test_that("test para - risk, risk_threshold",{
  
  #################################
  # risk
  
  # test: if(is.vector(risk)){ if(length(risk) == nrow(data)) }
  expect_error(recordSwap(data = dat, hid = hid, 
                          hierarchy = c("nuts1","nuts2"), # TESTING
                          similar = similar,
                          swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          risk = rep(1,dim(dat)[1]), # TESTING
                          risk_threshold = risk_threshold, 
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               "If risk is not a vector containing column indices or column names in data then risk must be either a data.table, data.frame or matrix!")
  
  # test: if(is.vector(risk)){ if(length(risk)!=length(hierarchy)) }
  expect_error(recordSwap(data = dat, hid = hid, 
                          hierarchy = c("nuts1","nuts2"), # TESTING
                          similar = similar,
                          swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          risk = 1, # TESTING
                          risk_threshold = risk_threshold, 
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               "risk and hierarchy need to address the same number of columns!")
  
  # test:  if(is.vector(risk)){ checkIndexString(risk,cnames,minLength = 1) }
  expect_error(recordSwap(data = dat, hid = hid, 
                          hierarchy = c("nuts1","nuts2"), # TESTING
                          similar = similar,
                          swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          risk = list("nuts1" = rep(1,dim(dat)[1]), 
                                      "nuts2" = rep(1,dim(dat)[1])
                          ), # TESTING
                          risk_threshold = risk_threshold, 
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               # "checkIndexString(risk, cnames, minLength = 1) : 
               "If risk is not a vector containing column indices or column names in data then risk must be either a data.table, data.frame or matrix!")
  
  # test: if(nrow(risk)>0){ if(ncol(risk)!=length(hierarchy)) }
  expect_error(recordSwap(data = dat, hid = hid, 
                          hierarchy = c("nuts1","nuts2"), # TESTING
                          similar = similar,
                          swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          risk = matrix(data = rep(1,3*dim(dat)[1]),
                                        ncol = 3
                          ), # TESTING
                          risk_threshold = risk_threshold, 
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               "number of columns in risk does not match number of hierarchies!")
  
  # test: if(is.null(cnamesrisk)){}else{ if(!any(cnamesrisk%in%cnames[hierarchy+1])){} }
  expect_error(recordSwap(data = dat, hid = hid, 
                          hierarchy = hier, # TESTING
                          similar = similar,
                          swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          risk = risk, # TESTING
                          risk_threshold = risk_threshold, 
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               "the columnnames of risk do not appear in data")
  
  # test: if(any(risk<0)||any(!unlist(lapply(risk, is.numeric))))
  expect_error(recordSwap(data = dat, hid = hid, 
                          hierarchy = hier, # TESTING
                          similar = similar,
                          swaprate = swaprate,
                          k_anonymity = k_anonymity,
                          risk_variables = risk_variables,
                          risk = matrix(
                            data = rep(-1,3*dim(dat)[1]),
                            ncol = 3
                          ), # TESTING
                          risk_threshold = risk_threshold, 
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               "risk must contain positive real values only!")
  
  #################################
  # risk_threshold
  
  # test: is.numeric(risk_threshold) 
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = k_anonymity,      
                          risk = risk, # TESTING
                          risk_threshold = "A", # TESTING
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               "risk_threshold must be a positiv numeric value")     
  
  # test: length(risk_threshold)==1    
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = k_anonymity,  
                          risk = risk, # TESTING
                          risk_threshold = c(0.5,2), # TESTING
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               "risk_threshold must be a positiv numeric value")     
  
  # test: risk_threshold>=0
  expect_error(recordSwap(data = dat, hid = hid, hierarchy = hier,
                          similar = similar, swaprate = swaprate,
                          k_anonymity = k_anonymity,       
                          risk = risk, # TESTING
                          risk_threshold = -1, # TESTING
                          risk_variables = risk_variables,
                          carry_along = NULL,
                          return_swapped_id = TRUE,
                          seed=seed),
               "risk_threshold must be a positiv numeric value")
  
})
