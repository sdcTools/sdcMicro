#################################
# test infoLoss
#

context("test infoLoss()")
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

data_s <- recordSwap(data = as.data.frame(dat), hid = hid, hierarchy = hier,
                      similar = similar, swaprate = swaprate,
                      k_anonymity = k_anonymity,
                      risk_variables = risk_variables,
                      carry_along = NULL,
                      return_swapped_id = TRUE,
                      seed=seed)

# test input parameter
test_that("test input parameter infoLoss()",{
  
  # data sets
  dat_wrong <- 1:10
  expect_error(infoLoss(data=dat_wrong, data_swapped = dat_s,
           table_vars = c("nuts2","national")),"`data` musst be a data.table or data.frame")
  expect_error(infoLoss(data=dat, data_swapped = dat_wrong,
                        table_vars = c("nuts2","national")),"`data_swapped` musst be a data.table or data.frame")
  
  # table_vars
  expect_error(infoLoss(data=dat, data_swapped = data_s,
                        table_vars = c("nuts2","natinal")),"not all variables in `table_vars` are column names of `data`")
  
  setnames(data_s,"national","national2")
  expect_error(infoLoss(data=dat, data_swapped = data_s,
                        table_vars = c("nuts2","national")),"not all variables in `table_vars` are column names of `data_swapped`")
  setnames(data_s,"national2","national")
  
  # metric
  expect_error(infoLoss(data=dat, data_swapped = data_s,
                        table_vars = c("nuts2","national"),
                        metric = c("abs2error")),"`metric` can only take values \"absD\", \"relabsD\" and \"abssqrtD\".\nUse parameter `custom_metric` if you want to apply additional metrices.")
  
  # hid
  expect_error(infoLoss(data=dat, data_swapped = data_s,
                        table_vars = c("nuts2","national"),
                        hid = "HID"),"`hid` must be a character vector of length 1 containing a column name in `data` and `data_swapped`")
  
  # probs
  expect_error(infoLoss(data=dat, data_swapped = data_s,
                        table_vars = c("nuts2","national"),
                        probs = 2),"`probs` must be a numeric vector with values in \\[0,1\\]")
  
  # quantvals
  expect_error(infoLoss(data=dat, data_swapped = data_s,
                        table_vars = c("nuts2","national"),
                        quantvals = c(-1)),"quanvals` must be a numeric vector of length 2 or more")
  
  # apply_quantvals
  expect_error(infoLoss(data=dat, data_swapped = data_s,
                        table_vars = c("nuts2","national"),
                        apply_quantvals = 1),"`apply_quantvals` must be a character vector containing names of the metric supplied in `metric` or `custom_metric`")
  
  # exclude_zeros
  expect_error(infoLoss(data=dat, data_swapped = data_s,
                        table_vars = c("nuts2","national"),
                        exclude_zeros = "YES"),"`exclude_zeros` must be TRUE or FALSE")
  
  # only_inner_cells
  expect_error(infoLoss(data=dat, data_swapped = data_s,
                        table_vars = c("nuts2","national"),
                        only_inner_cells = "YES"),"`only_inner_cells` must be TRUE or FALSE")
  
  # custom metric
  met2 <- function(x,y){
    sqrt(x^2+z^2)
  }
  expect_error(infoLoss(data=dat, data_swapped = data_s,
                        table_vars = c("nuts2","national"),
                        custom_metric = list(met2=met2)),"`custom_metric\\[\\[1\\]\\]` threw an error when calling `custom_metric\\[\\[1\\]\\]`\\(1:10,10:1\\): object 'z' not found")
  
  
})


test_that("test output infoLoss()",{
  
  #################################
  # swaprate
  met2 <- function(x,y){
    sqrt(x^2+y^2)
  }
  il <- infoLoss(data=dat, data_swapped = data_s,
                 table_vars = c("nuts2","national"),
                 custom_metric = list(met2=met2))
  
  # custom loss must be part of output
  expect_true("met2" %in% colnames(il$measures))
  
  # number of cells must be correct
  expect_true(nrow(il$cellvalues) == (nrow(dat[,.N,by=.(nuts2,national)])+uniqueN(dat$nuts2)+uniqueN(dat$national)))
  
  # counts must be correct
  dat_counts <- rbindlist(list(dat[,.(count_o=.N),by=.(nuts2,national)],
                          dat[,.(count_o=.N),by=.(nuts2)],
                          dat[,.(count_o=.N),by=.(national)]), use.name=TRUE,fill=TRUE)
  dat_counts_s <- rbindlist(list(data_s[,.(count_s=.N),by=.(nuts2,national)],
                               data_s[,.(count_s=.N),by=.(nuts2)],
                               data_s[,.(count_s=.N),by=.(national)]), use.name=TRUE,fill=TRUE)
  dat_counts <- merge(dat_counts,dat_counts_s,by=c("nuts2","national"))
  setkey(dat_counts,national,nuts2)
  setkey(il$cellvalues,national,nuts2)
  expect_true(all.equal(dat_counts,il$cellvalues))
  
  # check if metric is correctly applied
  x <- dat_counts[,abs(count_o-count_s)]
  x_q <- quantile(x,probs=seq(0, 1,.1))
  names(x_q)[c(1,6,11)] <- c("Min","Median","Max")
  expect_true(all(il$measures[what%in%names(x_q)]$absD==x_q))
  
  # check again with custom metric
  x <- dat_counts[,met2(count_o,count_s)]
  x_q <- quantile(x,probs=seq(0, 1,.1))
  names(x_q)[c(1,6,11)] <- c("Min","Median","Max")
  expect_true(all(il$measures[what%in%names(x_q)]$met2==x_q))
  
})

