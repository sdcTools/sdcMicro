#################################
# test recordSwap
#

context("test recordSwap() outputs")
library(sdcMicro)

seed <- 2021
set.seed(seed)
nhid <- 10000
dat <- sdcMicro::createDat( nhid )

k_anonymity <- 1
swaprate <- .05
similar <- list(c("hsize"))
hier <- c("nuts1","nuts2","nuts3")
risk_variables <- c("ageGroup","national")
hid <- "hid"
expect_swaps <- swaprate * nhid

# risk
risk <- matrix(
  data = rep(0.0001,3*dim(dat)[1]), # 0.0001 to swap all risky hd
  ncol = 3
) 
set.seed(seed)
index <- sample(1:(length(hier)*nrow(dat)), 
                size = expect_swaps)
risk[index] <- 99 # risk value
risk_threshold <- 0.9

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
  k_anonymity <- 2
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


test_that("test risk parameter - number of hd swaps)",{
  
  dat_s <- expect_warning(recordSwap(data = dat, hid = hid, hierarchy = hier,
                                     similar,
                                     risk = risk, # TESTING
                                     risk_threshold = risk_threshold, # TESTING
                                     carry_along = NULL,
                                     return_swapped_id = TRUE,
                                     seed=seed,
                                     swaprate = 0 # to test swaping based only on risk values
  ),"risk was adjusted in order to give each household member the maximum household risk value")
  
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
  
  # check if all risk households are swapped
  risk_check <- copy(data.table(risk))
  risk_variables_names <- copy(colnames(risk_check))
  risk_check[,hid_help:=dat[[5]]] # 5 = hid
  risk_check[,c(risk_variables_names):=lapply(.SD,max),
             .SDcols=c(risk_variables_names),
             by=.(hid_help)] # assign to each household its max value
  risk_check_uniq <- unique(risk_check
  ) # calculate max value in household
  risk_check_uniq[, Sum := rowSums(.SD), .SDcols = 1:3]
  n_of_hd_to_swap <- sum(risk_check_uniq$Sum > 1
  ) # number of households that should be swapped
  n_of_swapped_hd <- dat_s[hid != hid_swapped, uniqueN(hid)
  ] # number of swapped households
  
  # So what is difference in households
  hd_to_swap <- risk_check_uniq$hid_help[risk_check_uniq$Sum > 1
  ] # number of households that was should be swapped
  swapped_hd <- dat_s[hid != hid_swapped, unique(hid)
  ] # number of swapped households
  
  hd_check <- hd_to_swap[
    which(!(hd_to_swap %in% swapped_hd))
  ] # hd to swapp not in swapped hd
  # swapped_hd[
  #   which(!(swapped_hd %in% hd_to_swap))
  # ] # swapped hd not in hd to swap
  
  expect_true(is.integer(hd_check) && length(hd_check) == 0L)
  
})

test_that("test risk parameter - cpp)",{
  # In this test, we at first calculate risk value with internal function from 
  # cpp for the risk calculation. Secondly, we calculate risk values in the same 
  # way as they are calculated in cpp function. The values shoud be the same. 
  # After that, we feed recordSwap function with risk calculated in a custom way, 
  # and we compare it to the risk calculated internally. The values should be 
  # the same.
  
  
  # calculate risk with cpp function ------------------------------------------
  dat_t <- transpose(dat)
  cnames <- colnames(dat)
  hierarchy <- sdcMicro:::checkIndexString(hier, cnames = cnames, matchLength = length(hier)) 
  risk_var <- sdcMicro:::checkIndexString(risk_variables, cnames = cnames ,minLength = 1)
  hid_var <- sdcMicro:::checkIndexString(hid, cnames = cnames ,minLength = 1)  
  risk_cpp <- sdcMicro:::setRisk_cpp(dat_t,hierarchy,risk_var,hid_var)
  risk_cpp <- as.data.table(risk_cpp)
  risk_cpp <- transpose(risk_cpp)
  
  
  # calculate risk in same way as in cpp function  ----------------------------
  cnames <- colnames(dat)
  data <- dat
  hierarchy <- sdcMicro:::checkIndexString(hier, cnames = cnames, matchLength = length(hier)) +1
  risk_var <- sdcMicro:::checkIndexString(risk_variables, cnames = cnames ,minLength = 1) +1
  hid_var <- sdcMicro:::checkIndexString(hid, cnames = cnames ,minLength = 1) +1
  
  # initialise parameters
  n <- nrow(dat)
  nhier <- length(hierarchy)
  nrisk <- length(risk_variables)
  
  risk_calc <- data.table(
    nuts1 = rep(NA,n),
    nuts2 = rep(NA,n),
    nuts3 = rep(NA,n)
  )
  
  for(h in 1:nhier){
    # ---
    dat_reg <- data[,c(hierarchy[h],
                       risk_var),
                    with = FALSE]
    names(dat_reg)[1] <- "region"
    dat_reg$risk_comb <- paste(dat_reg$region, dat_reg$ageGroup, dat_reg$national,
                               sep = "_")
    uniqueness <- data.table(table(dat_reg$risk_comb))
    dat_reg <- merge(x = dat_reg,
                     y = uniqueness,
                     by.x = "risk_comb",
                     by.y = "V1",
                     sort = FALSE
    )
    dat_reg$risk <- 1/dat_reg$N
    
    risk_calc[,h] <- dat_reg$risk
  } # End of for cycle
  
  risk_calc[,hid:=data[[hid]]]
  risk_calc2 <- copy(risk_calc)
  risk_calc[,c(hier):=lapply(.SD,max),
            .SDcols=c(hier),
            by=.(hid)]
  risk_calc[,hid:=NULL]
  
  expect_true(all(risk_calc == risk_cpp))
  
  
  # Test if the results are the same
  dat_calc <- recordSwap(data = dat, hid = hid, hierarchy = hier,
                         similar = similar, 
                         risk = risk_calc, # TESTING
                         risk_threshold = 0.5, # TESTING
                         swaprate = 0, # to test swaping based only on risk values
                         carry_along = NULL,
                         return_swapped_id = TRUE,
                         seed=seed
  )
  dat_s <- recordSwap(data = dat, hid = hid, hierarchy = hier,
                      similar = similar, 
                      risk_variables = risk_variables, # TESTING
                      k_anonymity = 2, # TESTING
                      swaprate = 0, # to test swaping based only on risk values
                      carry_along = NULL,
                      return_swapped_id = TRUE,
                      seed=seed
  )
  expect_true(all(dat_calc==dat_s))
  
})
