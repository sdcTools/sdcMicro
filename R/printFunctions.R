setGeneric('freq', function(obj, type="fk") {standardGeneric('freq')})
setMethod(f='freq', signature=c('sdcMicroObj'),
    definition=function(obj,type="fk") { 
    if(type=="fk")
       ret <- obj@risk$individual[,2]
     else if(type=="Fk")
       ret <- obj@risk$individual[,3]
     else 
       stop(paste("type=",type,"is unknown."))
     return(ret)
 })


setMethod(f='print', signature=c('sdcMicroObj'),
    definition=function(x,type="freq",...) {
      obj=x
      #type=freq,ls,recode,risk,numrisk
      TFchange <- TRUE
      if(identical(obj@risk$individual,obj@originalRisk$individual))
        TFchange <- FALSE
      if(type=="freq"){
        cat("Number of observations violating\n")
        cat("\n -  2-anonymity:  ")
        if(TFchange)
          cat(paste(sum(obj@risk$individual[,2]<2),
                "(orig: ",sum(obj@originalRisk$individual[,2]<2),")\n"))
        else
          cat(paste(sum(obj@risk$individual[,2]<2),"\n"))
        cat(" -  3-anonymity:  ")
        if(TFchange)
          cat(paste(sum(obj@risk$individual[,2]<3),
                "(orig: ",sum(obj@originalRisk$individual[,2]<3),")"))
        else
          cat(paste(sum(obj@risk$individual[,2]<3),""))
        cat("\n--------------------------\n")  
        n <- nrow(obj@origData)
        cat("\nPercentage of observations violating\n")
        cat(" -  2-anonymity:  ")
        if(TFchange)
          cat(paste(round(sum(obj@risk$individual[,2]<2)/n*100,2),"% ",
                "(orig: ",round(sum(obj@originalRisk$individual[,2]<2)/n*100,2),"%",")\n"))
        else
          cat(paste(round(sum(obj@risk$individual[,2]<2)/n*100,2),"% \n"))
        cat(" -  3-anonymity:  ")
        if(TFchange)
          cat(paste(round(sum(obj@risk$individual[,2]<3)/n*100,2),"% ",
                "(orig: ",round(sum(obj@originalRisk$individual[,2]<3)/n*100,2),"%",")"))
        else
          cat(paste(round(sum(obj@risk$individual[,2]<3)/n*100,2),"% \n"))
      }else if(type=="risk"){
        risk <- obj@risk
        originalRisk <- obj@originalRisk
        cat("\n")
        cat("--------------------------\n")
        s <- sum((risk$individual[,1] > median(risk$individual[,1])+2*mad(risk$individual[,1])) & (risk$indiviual[,1] > 0.1))
        sorig <- sum((originalRisk$individual[,1] > median(originalRisk$individual[,1])+2*mad(originalRisk$individual[,1])) & (originalRisk$indiviual[,1] > 0.1))
        if(TFchange)
          cat(paste(s," (orig:", sorig, ")","obs. with higher risk than the main part\n"))
        else
          cat(paste(s,"obs. with higher risk than the main part\n"))
        cat("Expected no. of re-identifications:\n",round(risk$global$risk_ER,2),"")
        if(TFchange)
          cat("[",round(risk$global$risk_pct,2),"%]  (orig:", round(originalRisk$global$risk_ER,2), 
            "[",round(originalRisk$global$risk_pct,2),"%])\n")
        else
          cat("[",round(risk$global$risk_pct,2),"%]\n")
        cat("--------------------------\n")
        if("hier_risk_ER"%in%names(risk$global)){
          if(!is.na(risk$global$hier_risk_ER)){
            cat("--------------------------\n")
            cat("Hierarchical risk \n")
            cat("--------------------------\n")
            cat("Expected no. of re-identifications:\n",
                round(risk$global$hier_risk_ER,2),"")
            if(TFchange)
              cat("[",round(risk$global$hier_risk_pct,2),"%]  (orig:", 
                round(originalRisk$global$hier_risk_ER,2), 
                "[",round(originalRisk$global$hier_risk_pct,2),"%])\n")
            else
              cat("[",round(risk$global$hier_risk_pct,2),"%]  \n")
          }else{
            cat("--------------------------\n")
            cat("Hierarchical risk not available\n")
            cat("--------------------------\n")
          }
        }
      } else if ( type == "pram") {
		  pp <- get.sdcMicroObj(obj, type="pram")
		  if ( is.null(pp) ) {
			  cat("PRAM has not been applied!\n")
		  } else {
			  cat("Number of changed observations: \n")
			  cat("- - - - - - - - - - - \n")		  
			  print(pp$summary)			  
		  }
	  } else if(type=="ls"){
        keyVars <- colnames(obj@manipKeyVars)
        maxnam <- max(sapply(keyVars, nchar))
        if(is.null(obj@localSuppression))
          lsup <- list(rep(0,length(keyVars)))
        else
          lsup <- obj@localSuppression
        for(i in 1:length(keyVars)){
          nam <- keyVars[i]
          n <- nrow(obj@origData)
          cat("\n")
          cat(keyVars[i],paste(rep(".",2+maxnam-nchar(nam)), collapse=""),lsup[[1]][i])
          cat(" [", round(100*lsup[[1]][i]/n,3), "%]\n")
        }
      }else if(type=="recode"){
        cat("Reported is the")
        cat("\n")
        cat(" number | mean size and | size of smallest category")
        cat("\n")
        k <- length(obj@keyVars)
        tab <- tab2 <- ssize <- ssize2 <- msize <- msize2 <- numeric(k)
        names(tab) <- colnames(obj@origData[,obj@keyVars])
        cat("\n")
        for(i in 1:k){
          tab2[i] <- length(unique(obj@origData[,obj@keyVars[i]]))
          tab[i] <- length(unique(obj@manipKeyVars[,i]))
          t2 <- table(obj@origData[,obj@keyVars[i]])
          t1 <- table(obj@manipKeyVars[,i])
          msize[i] <- round(mean(t1),0)
          msize2[i] <- round(mean(t2),0)
          ssize[i] <- min(t1)
          ssize2[i] <- min(t2)
        }
        nc <- sapply(names(tab), nchar)
        maxnam <- max(nc)
        for(i in 1:k){
          nam <- names(tab)[i]
          cat("-------------\n")
          if(TFchange)
            cat(nam, paste(rep(".",2+maxnam-nchar(nam)), collapse=""),tab[i],"|",msize[i],"|",ssize[i], 
              "\n     (orig:", tab2[i],"|",msize2[i],"|",ssize2[i],") \n")
          else
            cat(nam, paste(rep(".",2+maxnam-nchar(nam)), collapse=""),tab[i],"|",msize[i],"|",ssize[i], 
                "\n ")
        }
      }else if(type=="numrisk"){
        risk <- obj@risk
        utility <- obj@utility 
        if(TFchange){
          cat(paste("Disclosure Risk is between: \n [0% ; ", 
            round(100*risk$numeric,2), "%] (current)\n 
                (orig: ~", 100, "%) \n",sep=""))
          cat(paste("- Information Loss:\n    IL1: ", 
             round(utility$il1,2),"\n  - Difference Eigenvalues: ",round(utility$eigen*100,2)," %",
        "\n\n (orig: Information Loss: 0) \n",sep=""))
       }else{
          cat(paste("Disclosure Risk is between: \n [0% ; ", 
                  round(100*risk$numeric,2), "%] (current)\n ",sep=""))
          cat(paste("- Information Loss:\n    IL1: ", 
                  round(utility$il1,2),"\n  - Difference Eigenvalues: ",round(utility$eigen*100,2)," %",
                  "\n \n",sep=""))
       }
        
        
      }else
        stop(paste("type=",type,"is unknown."))
    })