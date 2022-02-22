#################################
# test recordSwap
#

context("test recordSwap() outputs")
library(sdcMicro)

seed <- 2021
set.seed(seed)
nhid <- 10000
dat <- sdcMicro::createDat( nhid )

k_anonymity <- 0
swaprate <- .05
similar <- list(c("hsize"))
hier <- c("nuts1","nuts2","nuts3")
risk_variables <- c("ageGroup","national")
hid <- "hid"
expect_swaps <- swaprate * nhid

# test input parameter
test_that("test households correctly swapped",{
  
  dat_s <- recordSwap(data = dat, hid = hid, hierarchy = hier,
                      similar = similar, swaprate = swaprate,
                      k_anonymity = k_anonymity,
                      risk_variables = risk_variables,
                      carry_along = NULL,
                      return_swapped_id = TRUE,
                      seed=seed)
  
  expect_true(dat_s[hid!=hid_swapped,uniqueN(hid)]==expect_swaps)
  
  # check that swapped regions are identical for each household
  dat_check <- dat_s[,lapply(.SD,uniqueN),by=.(hid),.SDcols=c(hier)]
  expect_true(all(dat_check[,!..hid]==1))
  
  # check that region was actually swapped correctly
  dat1 <- unique(dat[,.SD,.SDcols=c(hid,hier)])
  dat1[,region:=do.call(paste,.SD),.SDcols=c(hier)]
  dat2 <- unique(dat_s[,.SD,.SDcols=c(hier,paste0(hid,"_swapped"))])
  dat2[,region2:=do.call(paste,.SD),.SDcols=c(hier)]
  setnames(dat2,paste0(hid,"_swapped"),hid)
  dat_check <- merge(dat1,dat2,by="hid")
  expect_true(all(dat_check[,region==region2]))
  
  # check that data is identical on all but swapped hierarchies
  # the order of the actual data should not be swapped
  # since data was already sorted by hid
  drop_cols <- hier
  drop_cols2 <- c(hier,paste0(hid,"_swapped"))
  expect_true(all.equal(dat[,!..drop_cols],dat_s[,!..drop_cols2]))

})


# test input parameter
test_that("test number of swapped households",{
  
  # swaprate 10%
  dat_s <- recordSwap(data = dat, hid = hid, hierarchy = hier,
                      similar = similar, swaprate = .1,
                      k_anonymity = k_anonymity,
                      risk_variables = risk_variables,
                      carry_along = NULL,
                      return_swapped_id = TRUE,
                      seed=seed)
  
  expect_true(dat_s[hid!=hid_swapped,uniqueN(hid)]==(.1*nhid))
  
  # swaprate 25%
  dat_s <- recordSwap(data = dat, hid = hid, hierarchy = hier,
                      similar = similar, swaprate = .25,
                      k_anonymity = k_anonymity,
                      risk_variables = risk_variables,
                      carry_along = NULL,
                      return_swapped_id = TRUE,
                      seed=seed)
  
  expect_true(dat_s[hid!=hid_swapped,uniqueN(hid)]==(.25*nhid))
})

test_that("test carry along output",{
  carry_along <- c("lau2")
  dat_s <- recordSwap(data = dat, hid = hid, hierarchy = hier,
                      similar = similar, swaprate = swaprate,
                      k_anonymity = k_anonymity,
                      risk_variables = risk_variables,
                      carry_along = carry_along,
                      return_swapped_id = TRUE,
                      seed=seed)

  # check that swapped regions and carry along are identical for each household
  dat_check <- dat_s[,lapply(.SD,uniqueN),by=.(hid),.SDcols=c(hier,carry_along)]
  expect_true(all(dat_check[,!..hid]==1))
  
  # check that region was actually swapped correctly
  dat1 <- unique(dat[,.SD,.SDcols=c(hid,hier,carry_along)])
  dat1[,region:=do.call(paste,.SD),.SDcols=c(hier,carry_along)]
  dat2 <- unique(dat_s[,.SD,.SDcols=c(hier,carry_along,paste0(hid,"_swapped"))])
  dat2[,region2:=do.call(paste,.SD),.SDcols=c(hier,carry_along)]
  setnames(dat2,paste0(hid,"_swapped"),hid)
  dat_check <- merge(dat1,dat2,by="hid")
  expect_true(all(dat_check[,region==region2]))
  
  # check that data is identical on all but swapped hierarchies
  # the order of the actual data should not be swapped
  # since data was already sorted by hid
  drop_cols <- c(hier,carry_along)
  drop_cols2 <- c(hier,carry_along,paste0(hid,"_swapped"))
  expect_true(all.equal(dat[,!..drop_cols],dat_s[,!..drop_cols2]))
})


test_that("test k anonymity levels",{
  
  hier <- c("nuts1","nuts2")
  risk_variables <- c("ageGroup","national","hsize")
  k_anonymity <- 1
  # check if all risk households are swapped
  dat_s <- recordSwap(data = dat, hid = hid, hierarchy = hier,
                      similar = similar, swaprate = swaprate,
                      k_anonymity = k_anonymity,
                      risk_variables = risk_variables,
                      carry_along = NULL,
                      return_swapped_id = TRUE,
                      seed=seed)
  expect_true(dat_s[hid!=hid_swapped,uniqueN(hid)]>=expect_swaps)
  
  # check if high risk households were swapped
  dat_t <- transpose(dat)
  cnames <- colnames(dat)
  hierarchy <- sdcMicro:::checkIndexString(hier, cnames = cnames, matchLength = length(hier))
  risk_var <- sdcMicro:::checkIndexString(risk_variables, cnames = cnames ,minLength = 1)
  hid_var <- sdcMicro:::checkIndexString(hid, cnames = cnames ,minLength = 1)  
  risk <- sdcMicro:::setRisk_cpp(dat_t,hierarchy,risk_var,hid_var)
  risk_threshold <- 1/k_anonymity
  dat_s$levels <- sdcMicro:::setLevels_cpp(risk,risk_threshold)
  table(dat_s$levels)
  expect_true(all(dat_s[levels>2,all(hid!=hid_swapped)]))
  
  # use different k
  k_anonymity <- 3
  dat_s2 <- recordSwap(data = dat, hid = hid, hierarchy = hier,
                      similar = similar, swaprate = swaprate,
                      k_anonymity = k_anonymity,
                      risk_variables = risk_variables,
                      carry_along = NULL,
                      return_swapped_id = TRUE,
                      seed=seed)
  expect_true(dat_s2[hid!=hid_swapped,uniqueN(hid)]>=expect_swaps)
  expect_true(dat_s2[hid!=hid_swapped,uniqueN(hid)]>=dat_s[hid!=hid_swapped,uniqueN(hid)])
  
  # check if households at high risk at highest level were swapped at highest level
  dat_t <- transpose(dat)
  cnames <- colnames(dat)
  hierarchy <- sdcMicro:::checkIndexString(hier, cnames = cnames, matchLength = length(hier))
  risk_var <- sdcMicro:::checkIndexString(risk_variables, cnames = cnames ,minLength = 1)
  hid_var <- sdcMicro:::checkIndexString(hid, cnames = cnames ,minLength = 1)  
  risk <- sdcMicro:::setRisk_cpp(dat_t,hierarchy,risk_var,hid_var)
  risk_threshold <- 1/k_anonymity
  dat_s2$levels <- sdcMicro:::setLevels_cpp(risk,risk_threshold)
  
  i <- dat_s2[levels==0][[hid]]
  dat_help <- dat[.(i),on=c(hid)][,.SD,.SDcols=c(hid,hier)]
  setnames(dat_help,c(hier),c(paste0(hier,"_orig")))
  dat_s2_check <- merge(unique(dat_s2[levels==0],by=hid),
                        unique(dat_help,by=hid), by=hid)
  hl <- hier[1]
  hl2 <- paste0(hl,"_orig")
  
  expect_true(all(dat_s2_check[,get(hl)!=get(hl2)]))
  
})


test_that("test similarity profiles",{
  
  hier <- c("nuts1","nuts2")
  similar <- list(c("hsize"),
                  c("hsize","htype"))
  
  # check if all risk households are swapped
  dat_s <- recordSwap(data = dat, hid = hid, hierarchy = hier,
                      similar = similar, swaprate = swaprate,
                      k_anonymity = 0,
                      risk_variables = risk_variables,
                      carry_along = NULL,
                      return_swapped_id = TRUE,
                      seed=seed)
  expect_true(dat_s[hid!=hid_swapped,uniqueN(hid)]>=expect_swaps)
  
  # check if similarity variables were uphold during swapping
  sim_vars <- unique(unlist(similar))
  sim_vars_sw <- c(paste0(sim_vars,"_swapped"))
  i <- unique(dat_s[hid!=hid_swapped][["hid_swapped"]])
  dat_help <- dat[.(i),on=c(hid)][,.SD,.SDcols=c(hid,sim_vars)]
  setnames(dat_help,sim_vars,sim_vars_sw)
  dat_s_check <- merge(unique(dat_s[hid!=hid_swapped],by=hid),
                       unique(dat_help,by=hid), by=hid)
  
  check_sim <- sapply(1:length(sim_vars),function(z){
    dat_s_check[,all(get(sim_vars[z])==get(sim_vars_sw[z]))]
  })
  expect_true(all(check_sim))  
})


