require(sdcMicro); data(testdata)
sdc <- createSdcObj(testdata, 
		keyVars=c('urbrur','water','sex','age'), 
		numVars=c('expend','income','savings'), 
		w='sampling_weight', hhId='ori_hid')

p1 <- print(sdc)
sdc <- globalRecode(sdc, column="age",
		breaks=c(1,9,19,29,39,49,59,69,100), labels=1:8)
p2 <- print(sdc)

if(identical(p1,p2)) stop("frequency counts are not updated")