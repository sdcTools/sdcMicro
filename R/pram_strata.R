setGeneric('pram_strata', function(obj, variables=NULL,strata_variables=NULL,
				weights=NULL,seed=NULL,missing=-999, pd=0.8, alpha=0.5) {standardGeneric('pram_strata')})
setMethod(f='pram_strata', signature=c('sdcMicroObj'),
	definition=function(obj, variables=NULL,strata_variables=NULL,
		weights=NULL,seed=NULL,missing=-999, pd=0.8, alpha=0.5) { 
	
	### Get data from manipPramVars
	manipData <- get.sdcMicroObj(obj, type="manipPramVars")
	pramVars <- colnames(obj@origData)[get.sdcMicroObj(obj, type="pramVars")]
	strataVars <- get.sdcMicroObj(obj, type="strataVar")
	if(length(strataVars)>0) {
		sData <- get.sdcMicroObj(obj, type="origData")[,strataVars,drop=F]
		manipData <- cbind(manipData, sData)
		strataVars <- c(length(pramVars):length(manipData))
	}
	
	res <- pram_strataWORK(data=manipData,variables=variables,
			strata_variables=strataVars,pd=pd,alpha=alpha, missing=missing,seed=seed,weights=weights)
	manipData[,variables] <- res[,paste(variables,"_pram",sep="")]
	obj <- nextSdcObj(obj)
	
	obj <- set.sdcMicroObj(obj, type="manipPramVars", input=list(manipData))
	
	pram <- get.sdcMicroObj(obj, type="pram")
	pram$pd <- pd
	pram$alpha <- alpha
	
	#rownames(result) <- 1:nrow(result)
	#colnames(result) <- c("transition", "Frequency")
	
	#pram$summary <- print
	pram$summary <- print.pram_strata(res)
	obj <- set.sdcMicroObj(obj, type="pram", input=list(pram))
	
	obj <- calcRisks(obj)
	
	obj
})
setMethod(f='pram_strata', signature=c("data.frame"),
	definition=function(obj, variables=NULL,strata_variables=NULL,
		weights=NULL,seed=NULL,missing=-999, pd=0.8, alpha=0.5) { 
	pram_strataWORK(data=obj, variables=variables,strata_variables=strata_variables,
		weights=weights,seed=seed,missing=missing,pd=pd,alpha=alpha)
})
setMethod(f='pram_strata', signature=c("matrix"),
	definition=function(obj, variables=NULL,strata_variables=NULL,
		weights=NULL,seed=NULL,missing=-999, pd=0.8, alpha=0.5) { 
	pram_strataWORK(data=obj, variables=variables,strata_variables=strata_variables,
		weights=weights,seed=seed,missing=missing,pd=pd,alpha=alpha)
})

#require(sdcMicro)
#data(testdata)
#dat <- testdata[,c("urbrur","sex","roof")]
#dat$roof_pram <- 0

# handling of NA and NULL Values for weight-vector
## with strata
#x <- .Call("Pram",as.matrix(dat),-999,2,1,-1)
## only frequency
#x <- .Call("Pram",as.matrix(dat),-999,0,0,-1)
pram_strataWORK <- function(data,variables=NULL,strata_variables=NULL,
		weights=NULL,seed=NULL,missing=-999, pd=0.8, alpha=0.5){
	if(is.null(variables))
		stop("Please define valid variables to pram!")
	if(length(strata_variables)>0){
		data$idvarpram <- 1:nrow(data)
		fac <- list()
		chara <- list()
		f <- rep("",nrow(data))
		for(sv in strata_variables){
			f <- paste(f,as.character(data[,sv]),"_",sep="")
		}
		f <- as.factor(f)
		s <- split(data[,c(variables,"idvarpram"),drop=FALSE],f)
		for(i in 1:length(variables)){
			v <- variables[i]
			fac[[i]] <- FALSE
			if(!is.factor(data[,v])&is.character(data[,v])){
				fac[[i]] <- TRUE
				chara[[i]] <- TRUE
			}else if(!is.factor(data[,v])&is.numeric(data[,v])){
				chara[[i]] <- FALSE
				data[,v] <- as.character(data[,v])
			}else{
				fac[[i]] <- TRUE
				data[,v] <- as.character(data[,v])
			}
			for(si in 1:length(s)){
				s[[si]][,paste(v,"_pram",sep="")] <- as.character(pram(as.factor(s[[si]][,v]),pd=pd,alpha=alpha)$xpramed)
			}
		}
		r <- vector()
		for(si in 1:length(s)){
			r <- rbind(r,s[[si]])
		}
		for(i in 1:length(variables)){
			v <- variables[i]
			if(!fac[[i]]){
				if(chara[[i]]){
					data[,v] <- as.character(data[,v])
					data[,paste(v,"_pram",sep="")] <- as.character(r[,paste(v,"_pram",sep="")])
				}else{
					data[,v] <- as.numeric(data[,v])
					data[,paste(v,"_pram",sep="")] <- as.numeric(r[,paste(v,"_pram",sep="")])
				}
			}else{
				data[,v] <- as.factor(data[,v])
				data[,paste(v,"_pram",sep="")] <- as.factor(r[,paste(v,"_pram",sep="")])
			}
			
		}
		data <- data[,-which(colnames(data)=="idvarpram"),drop=FALSE]
		
	}else{
		for(v in variables){
			if(is.factor(data[,v]))
				data[,paste(v,"_pram",sep="")] <- pram(data[,v],pd=pd,alpha=alpha)$xpramed
			else if(is.numeric(data[,v])){
				data[,paste(v,"_pram",sep="")] <- as.numeric(as.character(pram(as.factor(as.character(data[,v])),pd=pd,alpha=alpha)$xpramed))
			}else if(is.character(data[,v])){
				data[,paste(v,"_pram",sep="")] <- as.character(pram(as.factor(data[,v]),pd=pd,alpha=alpha)$xpramed)
			}
		}
	}
	res <- data
	#class(res) <- "pram_strata"
	invisible(res)
	
}

print.pram_strata <- function(x, ...){
	x <- apply(x, 2, as.character)
	x[is.na(x)] <- "." # NA comparisons -> cast to character (better solution?)
	pram_var <- colnames(x)[grep("pram",colnames(x))]
	var <- unlist(lapply(pram_var,function(x)substring(x,1,nchar(x)-5)))
	
	df <- data.frame(variable=var, nrChanges=NA, percChanges=NA, stringsAsFactors=FALSE)
	cat("Number of changed observations: \n")
	cat("- - - - - - - - - - - \n")
	for(i in 1:length(pram_var)){
		# FIXME: factor levels of x[,var[i]] and x[,pram_var[i]] not always the same!
		#s <- sum(x[,var[i]]!=x[,pram_var[i]])
		s <- sum(as.character(x[,var[i]])!=as.character(x[,pram_var[i]]))
		p <- round(s/nrow(x)*100,2)
		cat(var[i]," != ",pram_var[i]," : ",s," (",p,"%)","\n",sep="")
		df[i,"nrChanges"] <- s
		df[i,"percChanges"] <- p
	}
	return(invisible(df))
}
