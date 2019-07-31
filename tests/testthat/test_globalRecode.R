library(testthat)




# data frame
{
	set.seed(3)
	sample_Data <- runif(10, 1, 100)
	sample_Data <- cbind(sample_Data, as.factor(round(runif(10, 1, 4), digits = 0)))
	sample_Data <- as.data.frame(sample_Data)
	colnames(sample_Data) <- c("Var1", "Var2")
	sample_Data[,2] <- as.factor(sample_Data[,2])


	# Continuous Variable
	{
		# breaks = 1
		{
			test_that("numeric values (breaks = 1, labels = yes, method = equidistant)", {
			  expect_error(globalRecode(sample_Data[,1], breaks = 1, labels = 1, method = "equidistant"), "invalid number of intervals")  
			})

			test_that("numeric values (breaks = 1, labels = yes, method = logEqui)", {
			  fr <- globalRecode(sample_Data[,1], breaks = 1, labels = 1, method = "logEqui")
			  A <- levels(fr)[1]
			  expect_equal(all(fr == c(A,A,A,A,A,A,A,A,A,A)), TRUE)  
			})

			test_that("numeric values (breaks = 1, labels = yes, method = equalAmount)", {
			  fr <- globalRecode(sample_Data[,1], breaks = 1, labels = 1, method = "equalAmount")
			  A <- levels(fr)[1]
			  expect_equal(all(fr == c(A,A,A,A,A,A,A,A,A,A)), TRUE)  
			})


			test_that("numeric values (breaks = 1, labels = no, method = equidistant)", {
			  expect_error(globalRecode(sample_Data[,1], breaks = 1, labels = 1, method = "equidistant"), "invalid number of intervals")  
			})

			test_that("numeric values (breaks = 1, labels = no, method = logEqui)", {
			  fr <- globalRecode(sample_Data[,1], breaks = 1, method = "logEqui")
			  A <- levels(fr)[1]
			  expect_equal(all(fr == c(A,A,A,A,A,A,A,A,A,A)), TRUE)  
			})

			test_that("numeric values (breaks = 1, labels = no, method = equalAmount)", {
			  fr <- globalRecode(sample_Data[,1], breaks = 1, method = "equalAmount")
			  A <- levels(fr)[1]
			  expect_equal(all(fr == c(A,A,A,A,A,A,A,A,A,A)), TRUE)  
			})


			test_that("numeric values (breaks = 1, labels = invalid, method = equidistant)", {
			  expect_error(globalRecode(sample_Data[,1], breaks = 1, labels = 1:5, method = "equidistant"), "invalid number of intervals")  
			})

			test_that("numeric values (breaks = 1, labels = invalid, method = logEqui)", {
			  expect_error(globalRecode(sample_Data[,1], breaks = 1, labels = 1:5, method = "logEqui"), "lengths of 'breaks' and 'labels' differ")  
			})

			test_that("numeric values (breaks = 1, labels = invalid, method = equalAmount)", {
			  expect_error(globalRecode(sample_Data[,1], breaks = 1, labels = 1:5, method = "equalAmount"), "lengths of 'breaks' and 'labels' differ")  
			})
		}




		# breaks = more
		{
			test_that("numeric values (breaks = more, labels = yes, method = equidistant)", {
			  fr <- globalRecode(sample_Data[,1], breaks = 3, labels = 1:3, method = "equidistant")
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(A,C,B,A,C,C,A,A,B,C)), TRUE)
			})

			test_that("numeric values (breaks = more, labels = yes, method = logEqui)", {
			  fr <- globalRecode(sample_Data[,1], breaks = 3, labels = 1:3, method = "logEqui")
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(A,C,B,B,C,C,A,B,C,C)), TRUE)  
			})

			test_that("numeric values (breaks = more, labels = yes, method = equalAmount)", {
			  fr <- globalRecode(sample_Data[,1], breaks = 3, labels = 1:3, method = "equalAmount")
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(A,C,B,B,B,B,A,A,B,C)), TRUE)  
			})



			test_that("numeric values (breaks = more, labels = no, method = equidistant)", {
			  fr <- globalRecode(sample_Data[,1], breaks = 3, method = "equidistant")
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(A,C,B,A,C,C,A,A,B,C)), TRUE)
			})

			test_that("numeric values (breaks = more, labels = no, method = logEqui)", {
			  fr <- globalRecode(sample_Data[,1], breaks = 3, method = "logEqui")
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(A,C,B,B,C,C,A,B,C,C)), TRUE)  
			})

			test_that("numeric values (breaks = more, labels = no, method = equalAmount)", {
			  fr <- globalRecode(sample_Data[,1], breaks = 3, method = "equalAmount")
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(A,C,B,B,B,B,A,A,B,C)), TRUE)  
			})


			test_that("numeric values (breaks = more, labels = invalid, method = equidistant)", {
			  expect_error(globalRecode(sample_Data[,1], breaks = 3, labels = 1:5, method = "equidistant"), "lengths of 'breaks' and 'labels' differ")  
			})

			test_that("numeric values (breaks = more, labels = invalid, method = logEqui)", {
			  expect_error(globalRecode(sample_Data[,1], breaks = 3, labels = 1:5, method = "logEqui"), "lengths of 'breaks' and 'labels' differ")  
			})

			test_that("numeric values (breaks = more, labels = invalid, method = equalAmount)", {
			  expect_error(globalRecode(sample_Data[,1], breaks = 3, labels = 1:5, method = "equalAmount"), "lengths of 'breaks' and 'labels' differ")  
			})
		}




		# breaks = cutoff
		{
			test_that("numeric values (breaks = cutoff, labels = yes, method = none)", {
			  fr <- globalRecode(sample_Data[,1], breaks = c(0,30,60,90), labels=1:3)
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(A,C,B,B,C,C,A,B,B,C)), TRUE)
			})


			test_that("numeric values (breaks = cutoff, labels = no, method = none)", {
			  fr <- globalRecode(sample_Data[,1], breaks = c(0,30,60,90))
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(A,C,B,B,C,C,A,B,B,C)), TRUE)
			})


			test_that("numeric values (breaks = cutoff, labels = invalid, method = none)", {
			  expect_error(globalRecode(sample_Data[,1], breaks = c(0,30,60,90), labels = 1:5), "lengths of 'breaks' and 'labels' differ")  
			})
		}
	}









	# Categorical Variable
	{
		# breaks = 1
		{
			test_that("numeric values (breaks = 1, labels = yes, method = equidistant)", {
			  expect_error(globalRecode(sample_Data[,2], breaks = 1, labels = 1, method = "equidistant"), "invalid number of intervals")  
			})

			#test_that("numeric values (breaks = 1, labels = yes, method = logEqui)", {
			#  fr <- globalRecode(sample_Data[,2], breaks = 1, labels = 1, method = "logEqui")
			#  A <- levels(fr)[1]
			#  expect_equal(all(fr == c(A,A,A,A,A,A,A,A,A,A)), TRUE)  
			#})

			#test_that("numeric values (breaks = 1, labels = yes, method = equalAmount)", {
			#  fr <- globalRecode(sample_Data[,2], breaks = 1, labels = 1, method = "equalAmount")
			#  A <- levels(fr)[1]
			#  expect_equal(all(fr == c(A,A,A,A,A,A,A,A,A,A)), TRUE)  
			#})




			test_that("numeric values (breaks = 1, labels = no, method = equidistant)", {
			  expect_error(globalRecode(sample_Data[,2], breaks = 1, labels = 1, method = "equidistant"), "invalid number of intervals")  
			})

			#test_that("numeric values (breaks = 1, labels = no, method = logEqui)", {
			#  fr <- globalRecode(sample_Data[,2], breaks = 1, method = "logEqui")
			#  A <- levels(fr)[1]
			#  expect_equal(all(fr == c(A,A,A,A,A,A,A,A,A,A)), TRUE)  
			#})

			#test_that("numeric values (breaks = 1, labels = no, method = equalAmount)", {
			#  fr <- globalRecode(sample_Data[,2], breaks = 1, method = "equalAmount")
			# A <- levels(fr)[1]
			#  expect_equal(all(fr == c(A,A,A,A,A,A,A,A,A,A)), TRUE)  
			#})




			test_that("numeric values (breaks = 1, labels = invalid, method = equidistant)", {
			  expect_error(globalRecode(sample_Data[,2], breaks = 1, labels = 1:5, method = "equidistant"), "invalid number of intervals")  
			})

			test_that("numeric values (breaks = 1, labels = invalid, method = logEqui)", {
			  expect_error(globalRecode(sample_Data[,2], breaks = 1, labels = 1:5, method = "logEqui"), "lengths of 'breaks' and 'labels' differ")  
			})

			test_that("numeric values (breaks = 1, labels = invalid, method = equalAmount)", {
				expect_error(globalRecode(sample_Data[,2], breaks = 1, labels = 1:5, method = "equalAmount"), "lengths of 'breaks' and 'labels' differ")  
			})
		}




		# breaks = more
		{
			test_that("numeric values (breaks = more, labels = yes, method = equidistant)", {
			  fr <- globalRecode(sample_Data[,2], breaks = 3, labels = 1:3, method = "equidistant")
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(B,B,B,B,C,B,A,B,C,A)), TRUE)
			})

			#test_that("numeric values (breaks = more, labels = yes, method = logEqui)", {
			#  fr <- globalRecode(sample_Data[,2], breaks = 3, labels = 1:3, method = "logEqui")
			#  A <- levels(fr)[1]
			#  B <- levels(fr)[2]
			#  C <- levels(fr)[3]
			#  expect_equal(all(fr == c(A,C,B,B,C,C,A,B,C,C)), TRUE)  
			#})

			#test_that("numeric values (breaks = more, labels = yes, method = equalAmount)", {
			#  fr <- globalRecode(sample_Data[,2], breaks = 3, labels = 1:3, method = "equalAmount")
			#  A <- levels(fr)[1]
			#  B <- levels(fr)[2]
			#  C <- levels(fr)[3]
			#  expect_equal(all(fr == c(A,C,B,B,B,B,A,A,B,C)), TRUE)  
			#})



			test_that("numeric values (breaks = more, labels = no, method = equidistant)", {
			  fr <- globalRecode(sample_Data[,2], breaks = 3, method = "equidistant")
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(B,B,B,B,C,B,A,B,C,A)), TRUE)
			})

			#test_that("numeric values (breaks = more, labels = no, method = logEqui)", {
			#  fr <- globalRecode(sample_Data[,2], breaks = 3, method = "logEqui")
			#  A <- levels(fr)[1]
			#  B <- levels(fr)[2]
			#  C <- levels(fr)[3]
			#  expect_equal(all(fr == c(A,C,B,B,C,C,A,B,C,C)), TRUE)  
			#})

			#test_that("numeric values (breaks = more, labels = no, method = equalAmount)", {
			#  fr <- globalRecode(sample_Data[,2], breaks = 3, method = "equalAmount")
			# A <- levels(fr)[1]
			#  B <- levels(fr)[2]
			#  C <- levels(fr)[3]
			#  expect_equal(all(fr == c(A,C,B,B,B,B,A,A,B,C)), TRUE)  
			#})


			test_that("numeric values (breaks = more, labels = invalid, method = equidistant)", {
			  expect_error(globalRecode(sample_Data[,2], breaks = 3, labels = 1:5, method = "equidistant"), "lengths of 'breaks' and 'labels' differ")  
			})

			test_that("numeric values (breaks = more, labels = invalid, method = logEqui)", {
			  expect_error(globalRecode(sample_Data[,2], breaks = 3, labels = 1:5, method = "logEqui"), "lengths of 'breaks' and 'labels' differ")  
			})

			#test_that("numeric values (breaks = more, labels = invalid, method = equalAmount)", {
			#  expect_error(globalRecode(sample_Data[,2], breaks = 3, labels = 1:5, method = "equalAmount"), "lengths of 'breaks' and 'labels' differ")  
			#})
		}




		# breaks = cutoff
		{
			test_that("numeric values (breaks = cutoff, labels = yes, method = none)", {
			  fr <- globalRecode(sample_Data[,2], breaks = c(0,2,3,4), labels=1:3)
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(B,B,B,B,C,B,A,B,C,A)), TRUE)
			})


			test_that("numeric values (breaks = cutoff, labels = no, method = none)", {
			  fr <- globalRecode(sample_Data[,2], breaks = c(0,2,3,4))
			  A <- levels(fr)[1]
			  B <- levels(fr)[2]
			  C <- levels(fr)[3]
			  expect_equal(all(fr == c(B,B,B,B,C,B,A,B,C,A)), TRUE)
			})


			test_that("numeric values (breaks = cutoff, labels = invalid, method = none)", {
			  expect_error(globalRecode(sample_Data[,2], breaks = c(0,2,3,4), labels = 1:5), "lengths of 'breaks' and 'labels' differ")  
			})
		}
	}
}








# sdc
{
	set.seed(3)
	sample_Data <- runif(10, 1, 100)
	sample_Data <- cbind(sample_Data, as.factor(round(runif(10, 1, 4), digits = 0)))
	sample_Data <- as.data.frame(sample_Data)
	colnames(sample_Data) <- c("Var1", "Var2")
	sample_Data[,2] <- as.factor(sample_Data[,2])
	sample_Data <- createSdcObj(sample_Data, keyVars = c("Var2"), numVars = c("Var1"))


	# Continuous Variable
	{
		# breaks = 1
		{
			test_that("numeric values (breaks = 1, labels = yes, method = equidistant)", {
			  expect_error(globalRecode(sample_Data, breaks = 1, labels = 1, method = "equidistant"), "invalid number of intervals")  
			})

			test_that("numeric values (breaks = 1, labels = yes, method = logEqui)", {
			  sample_Data <- globalRecode(sample_Data, breaks = 1, labels = 1, method = "logEqui")
			  expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(1, 1, 1, 1, 1, 1, NA, 1, 1, 1))
			  expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
			  expect_equal(sample_Data@risk$global$risk, 0.1)
			  expect_equal(sample_Data@risk$global$risk_ER, 1.00)
			  expect_equal(sample_Data@risk$global$risk_pct, 10.00)
			  expect_equal(sample_Data@risk$global$threshold, 0.00)
			  expect_equal(sample_Data@risk$global$max_risk, 0.01)
			  expect_equal(sample_Data@risk$individual[,1], c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
			  expect_equal(sample_Data@risk$individual[,2], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
			  expect_equal(sample_Data@risk$individual[,3], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
			  expect_equal(sample_Data@risk$numeric, 1)
			})

			test_that("numeric values (breaks = 1, labels = yes, method = equalAmount)", {
			  sample_Data <- globalRecode(sample_Data, breaks = 1, labels = 1, method = "equalAmount")
			  expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(1, 1, 1, 1, 1, 1, NA, 1, 1, 1))
			  expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
			  expect_equal(sample_Data@risk$global$risk, 0.1)
			  expect_equal(sample_Data@risk$global$risk_ER, 1.00)
			  expect_equal(sample_Data@risk$global$risk_pct, 10.00)
			  expect_equal(sample_Data@risk$global$threshold, 0.00)
			  expect_equal(sample_Data@risk$global$max_risk, 0.01)
			  expect_equal(sample_Data@risk$individual[,1], c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
			  expect_equal(sample_Data@risk$individual[,2], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
			  expect_equal(sample_Data@risk$individual[,3], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
			  expect_equal(sample_Data@risk$numeric, 1)
			})

			test_that("numeric values (breaks = 1, labels = no, method = equidistant)", {
			  expect_error(globalRecode(sample_Data, breaks = 1, labels = 1, method = "equidistant"), "invalid number of intervals")  
			})

			test_that("numeric values (breaks = 1, labels = no, method = logEqui)", {
			  sample_Data <- globalRecode(sample_Data, breaks = 1, method = "logEqui")
			  expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(1, 1, 1, 1, 1, 1, NA, 1, 1, 1))
			  expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
			  expect_equal(sample_Data@risk$global$risk, 0.1)
			  expect_equal(sample_Data@risk$global$risk_ER, 1.00)
			  expect_equal(sample_Data@risk$global$risk_pct, 10.00)
			  expect_equal(sample_Data@risk$global$threshold, 0.00)
			  expect_equal(sample_Data@risk$global$max_risk, 0.01)
			  expect_equal(sample_Data@risk$individual[,1], c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
			  expect_equal(sample_Data@risk$individual[,2], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
			  expect_equal(sample_Data@risk$individual[,3], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
			  expect_equal(sample_Data@risk$numeric, 1)
			})

			test_that("numeric values (breaks = 1, labels = no, method = equalAmount)", {
			  sample_Data <- globalRecode(sample_Data, breaks = 1, method = "equalAmount")
			  expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(1, 1, 1, 1, 1, 1, NA, 1, 1, 1))
			  expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
			  expect_equal(sample_Data@risk$global$risk, 0.1)
			  expect_equal(sample_Data@risk$global$risk_ER, 1.00)
			  expect_equal(sample_Data@risk$global$risk_pct, 10.00)
			  expect_equal(sample_Data@risk$global$threshold, 0.00)
			  expect_equal(sample_Data@risk$global$max_risk, 0.01)
			  expect_equal(sample_Data@risk$individual[,1], c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
			  expect_equal(sample_Data@risk$individual[,2], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
			  expect_equal(sample_Data@risk$individual[,3], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
			  expect_equal(sample_Data@risk$numeric, 1)
			})

			test_that("numeric values (breaks = 1, labels = invalid, method = equidistant)", {
			  expect_error(globalRecode(sample_Data, breaks = 1, labels = 1:5, method = "equidistant"), "invalid number of intervals")  
			})

			test_that("numeric values (breaks = 1, labels = invalid, method = logEqui)", {
			  expect_error(globalRecode(sample_Data, breaks = 1, labels = 1:5, method = "logEqui"), "lengths of 'breaks' and 'labels' differ")  
			})

			test_that("numeric values (breaks = 1, labels = invalid, method = equalAmount)", {
			  expect_error(globalRecode(sample_Data, breaks = 1, labels = 1:5, method = "equalAmount"), "lengths of 'breaks' and 'labels' differ")  
			})
		}









		# breaks = more
		{
			test_that("numeric values (breaks = more, labels = yes, method = equidistant)", {
			  sample_Data <- globalRecode(sample_Data, breaks = 3, labels = 1:3, method = "equidistant")
			  expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(2, 2, 2, 2, 3, 2, 1, 2, 3, 1))
			  expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
			  expect_equal(sample_Data@risk$global$risk, 0.3)
			  expect_equal(sample_Data@risk$global$risk_ER, 3)
			  expect_equal(sample_Data@risk$global$risk_pct, 30)
			  expect_equal(sample_Data@risk$global$threshold, 0)
			  expect_equal(sample_Data@risk$global$max_risk, 0.01)
			  expect_equal(round(sample_Data@risk$individual[,1], digits = 7), c(0.1666667, 0.1666667, 0.1666667, 0.1666667, 0.5000000, 0.1666667, 0.5000000, 0.1666667, 0.5000000, 0.5000000))
			  expect_equal(sample_Data@risk$individual[,2], c(6, 6, 6, 6, 2, 6, 2, 6, 2, 2))
			  expect_equal(sample_Data@risk$individual[,3], c(6, 6, 6, 6, 2, 6, 2, 6, 2, 2))
			  expect_equal(sample_Data@risk$numeric, 1)
			})

			test_that("numeric values (breaks = more, labels = yes, method = logEqui)", {
			  sample_Data <- globalRecode(sample_Data, breaks = 3, labels = 1:3, method = "logEqui")
			  expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(2, 2, 2, 2, 3, 2, NA, 2, 3, 1))
			  expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
			  expect_equal(round(sample_Data@risk$global$risk, digits = 3), 0.212)
			  expect_equal(round(sample_Data@risk$global$risk_ER, digits = 2), 2.12)
			  expect_equal(round(sample_Data@risk$global$risk_pct, digits = 1), 21.2)
			  expect_equal(sample_Data@risk$global$threshold, 0)
			  expect_equal(sample_Data@risk$global$max_risk, 0.01)
			  expect_equal(round(sample_Data@risk$individual[,1], digits = 7), c(0.1428571, 0.1428571, 0.1428571, 0.1428571, 0.3333333, 0.1428571, 0.1000000, 0.1428571, 0.3333333, 0.5000000))
			  expect_equal(sample_Data@risk$individual[,2], c(7, 7, 7, 7, 3, 7, 10, 7, 3, 2))
			  expect_equal(sample_Data@risk$individual[,3], c(7, 7, 7, 7, 3, 7, 10, 7, 3, 2))
			  expect_equal(sample_Data@risk$numeric, 1)
			})

			test_that("numeric values (breaks = more, labels = yes, method = equalAmount)", {
			  sample_Data <- globalRecode(sample_Data, breaks = 2, labels = 1:2, method = "equalAmount")
			  expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(1, 1, 1, 1, 2, 1, NA, 1, 2, 1))
			  expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
			  expect_equal(round(sample_Data@risk$global$risk, digits = 3), 0.164)
			  expect_equal(round(sample_Data@risk$global$risk_ER, digits = 2), 1.64)
			  expect_equal(round(sample_Data@risk$global$risk_pct, digits = 1), 16.4)
			  expect_equal(sample_Data@risk$global$threshold, 0)
			  expect_equal(sample_Data@risk$global$max_risk, 0.01)
			  expect_equal(round(sample_Data@risk$individual[,1], digits = 7), c(0.1250000, 0.1250000, 0.1250000, 0.1250000, 0.3333333, 0.1250000, 0.1000000, 0.1250000, 0.3333333, 0.1250000))
			  expect_equal(sample_Data@risk$individual[,2], c(8, 8, 8, 8, 3, 8, 10, 8, 3, 8))
			  expect_equal(sample_Data@risk$individual[,3], c(8, 8, 8, 8, 3, 8, 10, 8, 3, 8))
			  expect_equal(sample_Data@risk$numeric, 1)
			})



			test_that("numeric values (breaks = more, labels = no, method = equidistant)", {
			  sample_Data <- globalRecode(sample_Data, breaks = 3, method = "equidistant")
			  expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(2, 2, 2, 2, 3, 2, 1, 2, 3, 1))
			  expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
			  expect_equal(sample_Data@risk$global$risk, 0.3)
			  expect_equal(sample_Data@risk$global$risk_ER, 3)
			  expect_equal(sample_Data@risk$global$risk_pct, 30)
			  expect_equal(sample_Data@risk$global$threshold, 0)
			  expect_equal(sample_Data@risk$global$max_risk, 0.01)
			  expect_equal(round(sample_Data@risk$individual[,1], digits = 7), c(0.1666667, 0.1666667, 0.1666667, 0.1666667, 0.5000000, 0.1666667, 0.5000000, 0.1666667, 0.5000000, 0.5000000))
			  expect_equal(sample_Data@risk$individual[,2], c(6, 6, 6, 6, 2, 6, 2, 6, 2, 2))
			  expect_equal(sample_Data@risk$individual[,3], c(6, 6, 6, 6, 2, 6, 2, 6, 2, 2))
			  expect_equal(sample_Data@risk$numeric, 1)
			})

			test_that("numeric values (breaks = more, labels = no, method = logEqui)", {
			  sample_Data <- globalRecode(sample_Data, breaks = 3, method = "logEqui")
			  expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(2, 2, 2, 2, 3, 2, NA, 2, 3, 1))
			  expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
			  expect_equal(round(sample_Data@risk$global$risk, digits = 3), 0.212)
			  expect_equal(round(sample_Data@risk$global$risk_ER, digits = 2), 2.12)
			  expect_equal(round(sample_Data@risk$global$risk_pct, digits = 1), 21.2)
			  expect_equal(sample_Data@risk$global$threshold, 0)
			  expect_equal(sample_Data@risk$global$max_risk, 0.01)
			  expect_equal(round(sample_Data@risk$individual[,1], digits = 7), c(0.1428571, 0.1428571, 0.1428571, 0.1428571, 0.3333333, 0.1428571, 0.1000000, 0.1428571, 0.3333333, 0.5000000))
			  expect_equal(sample_Data@risk$individual[,2], c(7, 7, 7, 7, 3, 7, 10, 7, 3, 2))
			  expect_equal(sample_Data@risk$individual[,3], c(7, 7, 7, 7, 3, 7, 10, 7, 3, 2))
			  expect_equal(sample_Data@risk$numeric, 1)
			})

			test_that("numeric values (breaks = more, labels = no, method = equalAmount)", {
			  sample_Data <- globalRecode(sample_Data, breaks = 2, method = "equalAmount")
			  expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(1, 1, 1, 1, 2, 1, NA, 1, 2, 1))
			  expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
			  expect_equal(round(sample_Data@risk$global$risk, digits = 3), 0.164)
			  expect_equal(round(sample_Data@risk$global$risk_ER, digits = 2), 1.64)
			  expect_equal(round(sample_Data@risk$global$risk_pct, digits = 1), 16.4)
			  expect_equal(sample_Data@risk$global$threshold, 0)
			  expect_equal(sample_Data@risk$global$max_risk, 0.01)
			  expect_equal(round(sample_Data@risk$individual[,1], digits = 7), c(0.1250000, 0.1250000, 0.1250000, 0.1250000, 0.3333333, 0.1250000, 0.1000000, 0.1250000, 0.3333333, 0.1250000))
			  expect_equal(sample_Data@risk$individual[,2], c(8, 8, 8, 8, 3, 8, 10, 8, 3, 8))
			  expect_equal(sample_Data@risk$individual[,3], c(8, 8, 8, 8, 3, 8, 10, 8, 3, 8))
			  expect_equal(sample_Data@risk$numeric, 1)
			})


			test_that("numeric values (breaks = more, labels = invalid, method = equidistant)", {
			  expect_error(globalRecode(sample_Data, breaks = 3, labels = 1:5, method = "equidistant"), "lengths of 'breaks' and 'labels' differ")  
			})

			test_that("numeric values (breaks = more, labels = invalid, method = logEqui)", {
			  expect_error(globalRecode(sample_Data, breaks = 3, labels = 1:5, method = "logEqui"), "lengths of 'breaks' and 'labels' differ")  
			})

			test_that("numeric values (breaks = more, labels = invalid, method = equalAmount)", {
			  expect_error(globalRecode(sample_Data, breaks = 2, labels = 1:5, method = "equalAmount"), "lengths of 'breaks' and 'labels' differ")  
			})
		}









		# breaks = cutoff
		{
			test_that("numeric values (breaks = cutoff, labels = yes, method = none)", {
				sample_Data <- globalRecode(sample_Data, breaks = c(0,30,60,90), labels=1:3)
				expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
				expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
				expect_equal(sample_Data@risk$global$risk, 0.1)
				expect_equal(sample_Data@risk$global$risk_ER, 1)
				expect_equal(sample_Data@risk$global$risk_pct, 10)
				expect_equal(sample_Data@risk$global$threshold, 0)
				expect_equal(sample_Data@risk$global$max_risk, 0.01)
				expect_equal(round(sample_Data@risk$individual[,1], digits = 1), c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
				expect_equal(sample_Data@risk$individual[,2], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
				expect_equal(sample_Data@risk$individual[,3], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
	   			expect_equal(sample_Data@risk$numeric, 1)
			})


			test_that("numeric values (breaks = cutoff, labels = no, method = none)", {
			  sample_Data <- globalRecode(sample_Data, breaks = c(0,30,60,90), labels=1:3)
				expect_equal(as.numeric(unlist(sample_Data@manipKeyVars)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
				expect_equal(round(as.numeric(unlist(sample_Data@manipNumVars)), digits = 5), c(17.63611, 80.94412, 39.10929, 33.44570, 60.60797, 60.83501, 13.33871, 30.16549, 58.18338, 63.46695))
				expect_equal(sample_Data@risk$global$risk, 0.1)
				expect_equal(sample_Data@risk$global$risk_ER, 1)
				expect_equal(sample_Data@risk$global$risk_pct, 10)
				expect_equal(sample_Data@risk$global$threshold, 0)
				expect_equal(sample_Data@risk$global$max_risk, 0.01)
				expect_equal(round(sample_Data@risk$individual[,1], digits = 1), c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
				expect_equal(sample_Data@risk$individual[,2], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
				expect_equal(sample_Data@risk$individual[,3], c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
	   			expect_equal(sample_Data@risk$numeric, 1)
			})


			test_that("numeric values (breaks = cutoff, labels = invalid, method = none)", {
			  expect_error(globalRecode(sample_Data, breaks = c(0,30,60,90), labels = 1:5), "lengths of 'breaks' and 'labels' differ")  
			})
		}
	}

}
