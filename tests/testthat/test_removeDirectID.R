set.seed(3)
sample_Data <- runif(20, 1, 100)
sample_Data <- array(sample_Data, dim = c(4,5))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 2), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 4), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 6), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 3), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 5), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 2), digits = 0))
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9", "Var10", "Var11")
sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
sample_Data[,7] <- as.factor(as.numeric(sample_Data[,7]))
sample_Data[,8] <- as.factor(as.numeric(sample_Data[,8]))
sample_Data[,9] <- as.factor(as.numeric(sample_Data[,9]))
sample_Data[,10] <- as.factor(as.numeric(sample_Data[,10]))
sample_Data[,11] <- as.factor(as.numeric(sample_Data[,11]))
sample_Data[c(1,3,4),6] <- NA
sample_Data[c(2,3),7] <- NA
sample_Data[c(3),8] <- NA
sample_Data[c(1,3),9] <- NA
sample_Data[c(2,3),10] <- NA
sample_Data[c(3),11] <- NA
sample_Data <- createSdcObj(sample_Data, keyVars = c("Var6", "Var7", "Var8", "Var9", "Var10", "Var11"), NumVars=c("Var1", "Var2", "Var3", "Var4", "Var5"))



removeDirectID(obj, var)