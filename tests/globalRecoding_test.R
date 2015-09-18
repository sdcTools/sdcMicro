require(sdcMicro)
data(testdata)
sdc <- createSdcObj(testdata, keyVars = c("urbrur", "water", "sex", "age"),
  numVars = c("expend", "income", "savings"), w = "sampling_weight", hhId = "ori_hid")

print(sdc)
sdc2 <- globalRecode(sdc, column = "age", breaks = c(1, 9, 19, 29, 39, 49, 59, 69, 100), labels = 1:8)
print(sdc2)

f1 <- get.sdcMicroObj(sdc, "risk")$individual[, "fk"]
f2 <- get.sdcMicroObj(sdc2, "risk")$individual[, "fk"]
if (identical(f1, f2)) stop("frequency counts are not updated")
