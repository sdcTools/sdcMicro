#################################
# test recordSwap() with a risk matrix and swaprate = 0 (issue #333)
#

library(sdcMicro)
library(data.table)

hier <- c("nuts1", "nuts2", "nuts3")

swap_risk <- function(dat, risk, seed, swaprate = 0) {
  recordSwap(
    data = copy(dat), hid = "hid", hierarchy = hier, similar = list("hsize"),
    risk = risk, risk_threshold = 0.9, return_swapped_id = TRUE,
    seed = seed, swaprate = swaprate, log_file_name = tempfile()
  )
}
swapped_hids <- function(d) {
  d[hid != hid_swapped, unique(hid)]
}

test_that("swaprate = 0 and no household at risk: nothing is swapped", {
  skip_on_cran()
  set.seed(1)
  dat <- createDat(500)
  # below the threshold but positive: every household is a possible donor
  risk <- matrix(0.1, nrow = nrow(dat), ncol = 3, dimnames = list(NULL, hier))
  n_swapped <- vapply(1:300, function(s) {
    length(swapped_hids(swap_risk(dat, risk, seed = s)))
  }, integer(1))
  expect_equal(sum(n_swapped > 0), 0)
})

test_that("swaprate = 0: only households at risk are swapped (example of issue #333)", {
  skip_on_cran()
  seed <- 2021
  set.seed(seed)
  dat <- createDat(10000)
  risk <- matrix(0, nrow = nrow(dat), ncol = 3, dimnames = list(NULL, hier))
  set.seed(seed)
  risk[sample(seq_len(length(hier) * nrow(dat)), size = 500)] <- 99

  expect_message(
    expect_warning(dat_s <- swap_risk(dat, risk, seed = seed), "risk was adjusted"),
    "Donor household was not found"
  )
  risky <- unique(dat$hid[rowSums(risk > 0.9) > 0])
  swapped <- swapped_hids(dat_s)
  expect_gt(length(swapped), 400)
  # donors count as swapped, too: no household with risk 0 may be among them
  expect_equal(setdiff(swapped, risky), integer(0))
  # swapping is pairwise
  pairs <- unique(dat_s[hid != hid_swapped, .(hid, hid_swapped)])
  expect_equal(sort(pairs$hid), sort(pairs$hid_swapped))
})

test_that("households with risk 0 are not used as donors", {
  set.seed(1)
  dat <- createDat(500)
  setkey(dat, hid)
  first_rows <- dat[, .SD[1], by = hid]
  last_hh <- first_rows[.N]
  # a household that could be swapped with the last household of the data
  cand <- first_rows[hsize == last_hh$hsize & nuts1 != last_hh$nuts1][1]

  risk <- matrix(0, nrow = nrow(dat), ncol = 3, dimnames = list(NULL, hier))
  risk[dat$hid == cand$hid, ] <- 99
  expect_message(dat_s <- swap_risk(dat, risk, seed = 1), "Donor household was not found in 1 case")
  expect_length(swapped_hids(dat_s), 0)

  # with a small positive risk the other households are possible donors
  risk[dat$hid != cand$hid, ] <- 1e-4
  dat_s <- swap_risk(dat, risk, seed = 1)
  swapped <- swapped_hids(dat_s)
  expect_length(swapped, 2)
  expect_true(cand$hid %in% swapped)
  donor <- setdiff(swapped, cand$hid)
  expect_equal(first_rows[hid == donor, hsize], cand$hsize)
  expect_false(first_rows[hid == donor, nuts1] == cand$nuts1)
})
