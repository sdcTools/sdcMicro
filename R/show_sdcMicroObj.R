setMethod(f="show", signature="sdcMicroObj",
    definition=function(object){
      dims <- dim(object@origData)
      cn <- colnames(object@origData)
      cat("Data set with",dims[1],"rows and",dims[2],"columns.\n")
      if(length(object@weightVar)>0){
        cat("Weight variable:",cn[object@weightVar],"\n") 
      }
      if(length(object@strataVar)>0){
          cat("Strata variable(s):",paste(cn[object@strataVar],collapse=", "),"\n") 
      }
      if(length(object@keyVars)>0){
        cat("\nCategorical key variables:",paste(cn[object@keyVars],collapse=", "),"\n")
        print(object,type="recode")
        cat("\n")
        print(object,type="freq")
        #cat("  Expected numbers of reidentification:",round(object@risk$global$risk_ER,2),"\n")
      }
      if(length(object@numVars)>0){
        cat("\n\nNumerical key variables:",paste(cn[object@numVars],collapse=", "),"\n")
        print(object,type="numrisk")
        #cat("  Numeric risk:",round(object@risk$numeric*100,2),"% \n")
      }
      if(!is.null(object@pram)){
        cat("\nPram(pd=",object@pram$pd,",alpha=",object@pram$alpha,") was applied:\n")
        print(object,type="pram")
        #print(object@pram$summary)
      }
      if(!is.null(object@localSuppression)){
        cat("\nLocal Suppression:\n")
        print(object,type="ls")
#        cnk <- cn[object@keyVars]
#        for(i in 1:length(cnk)){
#          cat(cnk[i],":",object@localSuppression[[1]][i],"\n")
#        }
      }
      
    }
)
