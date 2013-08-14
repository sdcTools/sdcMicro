#should be in zzz.R
#require(gWidgetsRGtk2)localSupp1_tmp
#sdcGUIenv <- new.env() starts in gui function

# just for test case while not in sdcMicro package
#require(sdcMicro)





sdcGUI <- function() {
  if(!is.null(options("quitRwithsdcGUI")[[1]]))#If started via windows binarybuild, auto start update
    updates2()
  updates22 <- function(...)updates2(restart=TRUE)
#Tooltip main window und select variables
  tt_selVar <- "Summary of the selected variables and their assignment"
  tt_print <- "Frequency print"
  tt_summary <- "Frequency summary"
  tt_ir <- "measure_risk Output"
  tt_vc <- "Configure your key variables"
  tt_ls1 <- "Local Suppression"
  tt_ld1 <- "Compute l-Diversity"
  tt_man <- "Run additional R commands"
  tt_pir <- "Histogram and ECDF of the individual risks"
  tt_noi <- "Add noise"
  tt_shuffle <- "Use model-based shuffling for generate anonym data"
  tt_ma <- "Microaggregation of numeric variables"
  tt_rr <- "Recalculate Risk"
  tt_slider1 <- "Paramter k for risk computation"
  tt_slider2 <- "Paramter k2 for risk computation"
  tt_nmr <- "Numerical method risk"
  tt_pram1 <- "PRAM is a probabilistic, perturbative method which can be applied on categorical variables"
  tt_genstrat <- "Generate a strata variable"
#
  mosaic_check <- function(formX){
    xtmp <- ActiveSdcVars("manipKeyVars")
    ft <- as.data.frame(ftable(xtmp[,formX,drop=FALSE]))
    ft <- ft[ft$Freq!=0,]
    if(nrow(ft)>40){ 
      plot(1,main="Two many classes for a nice mosaic plot!")
    }else{
      mosaic(as.formula(paste("~",paste(formX,collapse="+"),sep="")),data=xtmp,shade=FALSE)
    }
  }
  #data(free1)
  #data(testdata)
  # 
  
  .findHelpPage <- function(topic, package=NULL) {
    l <- list(topic=topic)
    if(!is.null(package))
      l$package <- package
    out <- do.call("help", l)
    if(length(out) == 0) return(NULL)
    
    pkgname <-  basename(dirname(dirname(out)))
    
    ## thanks to Josef L for this
    help.txt <- "" ## keep R CMD check happy  
    help.con <- textConnection("help.txt", "w", local = TRUE)
    tools::Rd2txt(utils:::.getHelpFile(out), out=help.con, package=pkgname,
        width=80L)
    close(help.con)
    
    return(list(x=help.txt,topic=topic, package=pkgname))
  }
  .insertHelpPage <- function(obj, x) {
    isSlow <- obj@toolkit@toolkit == "tcltk" || obj@toolkit@toolkit == "RGtk2"
    dispose(obj)       # clear
    
    out <- c()
    for(i in x) {
      if(grepl("^_\b",i)) {
        if(isSlow)
          out <- c(out, gsub("_\b","",i))
        else
          insert(obj, gsub("_\b","",i), font.attr=c(weight="bold"))
      } else {
        if(isSlow)
          out <- c(out,i)
        else
          insert(obj, i,font.attr=c(weight="normal"))
      }
    }
    if(isSlow)
      svalue(obj) <- out
    else
      insert(obj, "", do.newline=FALSE, where="beginning")              
  }
  helpR <- function(topic){
    print(help(topic))
  }
  
  
  # Script
  #
  Script <- function(name, ...) {
    if( missing(name) ) {
      getd("activeScript")
    } else { 
      putd("activeScript", name)
    }
  }
  
  Script.new <- function(...) {
    xtmp <- list(cmd=c())
    putd("activeScript", xtmp)
    cmd.seed <- paste("set.seed(",round(runif(1)*10e5),")")
    eval(parse(text=cmd.seed))
    Script.add(cmd.seed)
  }
  
  Script.add <- function(cmd, ...) {
    xtmp <- Script()
    xtmp$cmd[length(xtmp$cmd)+1] = cmd
    Script(xtmp)
  }
  
  Script.run <- function(xscr, ...) {
    #if( existd("activeDataSet") ) {
    if( TRUE ) {
      if( missing(xscr) ) {
        xcmd <- Script()
        xcmd <- xcmd$cmd
      } else {
        xcmd <- xscr
      }
      xprogress = gwindow("please wait", width=180, height=40, parent=window)
      glabel("... script running ...", container=xprogress)
      activedataset <- ""
      sdcObject <- ""
      for( i in 1:length(xcmd) ) {
        eval(parse(text=xcmd[i]))
        #xtmp <- function() { eval(parse(text=ytmp)) }
        #do.call(xtmp, list(), envir=sdcGUIenv)
      }
      putd("activeDataSet", activedataset)
      putd("dataSetName", "activedataset")
      ActiveSdcObject(sdcObject)
      writeVars(ActiveSdcVarsStr("keyVars"),ActiveSdcVarsStr("numVars"), ActiveSdcVarsStr("weightVar"),
          ActiveSdcVarsStr("hhId"),ActiveSdcVarsStr("strataVar"))
      dispose(xprogress)
    } else {
      gmessage("Run not possible, because no active data set found.", title="Attention", icon="error", parent=window)
    }
  }
  
  
  viewkanon <- function(){
    fk <- ActiveSdcVars("risk")$individual[,2]
    TFfk <- fk<3
    if(any(TFfk)){
      orig <- ActiveSdcVars("origData")
      kV <- ActiveSdcVars("manipKeyVars")
      nV <- ActiveSdcVars("manipNumVars")
      orig <- orig[,!colnames(orig)%in%c(colnames(kV),colnames(nV)),drop=FALSE]
      d <- orig
      if(!is.null(kV))
        d <- cbind(kV,orig)
      if(!is.null(nV))
        d <- cbind(nV,orig)
      xtmp <- cbind(fk[TFfk],d[TFfk,])
      colnames(xtmp)[1] <- c("fk")
      xtmp <- xtmp[order(xtmp[,1]),]
      win = gwindow("Observations violating 3-anoymity", parent=window)
      mainGroup1 = ggroup(container=win, horizontal=FALSE)
      vkT <- gtable(data.frame(apply(xtmp,2,function(x)as.character(x)),stringsAsFactors=FALSE))
      size(vkT) <- c(800,600)
      add(mainGroup1, vkT)
    }else
      gmessage("No observations violating 3-anonymity", title="Information", icon="info", parent=window)
  }
  viewldiv <- function(){
    ldiv <- ActiveSdcVars("risk")$ldiversity
    ldiv <- ldiv[,grep("_Distinct_Ldiversity",colnames(ldiv)),drop=FALSE]
    fk <- ActiveSdcVars("risk")$individual[,2]
    TFfk <- apply(ldiv,1,function(x)any(x<3))
    if(any(TFfk)){
      orig <- ActiveSdcVars("origData")
      kV <- ActiveSdcVars("manipKeyVars")
      nV <- ActiveSdcVars("manipNumVars")
      orig <- orig[,!colnames(orig)%in%c(colnames(kV),colnames(nV)),drop=FALSE]
      d <- orig
      if(!is.null(kV))
        d <- cbind(kV,orig)
      if(!is.null(nV))
        d <- cbind(nV,orig)
      xtmp <- cbind(ldiv[TFfk,],fk[TFfk],d[TFfk,])
      colnames(xtmp)[1:ncol(ldiv)] <- colnames(ldiv)
      colnames(xtmp)[ncol(ldiv)+1] <- "fk"
      xtmp <- xtmp[order(xtmp[,1]),]
      win = gwindow("Observations violating 2 l-diversity", parent=window)
      mainGroup1 = ggroup(container=win, horizontal=FALSE)
      vkT <- gtable(data.frame(apply(xtmp,2,function(x)as.character(x)),stringsAsFactors=FALSE))
      size(vkT) <- c(800,600)
      add(mainGroup1, vkT)
    }else
      gmessage("No observations violating 2 l-diversity", title="Information", icon="info", parent=window)
  }
  viewhigh <- function(){
    rk <- ActiveSdcVars("risk")$individual[,1]
    rko <- order(rk,decreasing = TRUE)[1:20]
    fk <- ActiveSdcVars("risk")$individual[,2]
    orig <- ActiveSdcVars("origData")
    kV <- ActiveSdcVars("manipKeyVars")
    nV <- ActiveSdcVars("manipNumVars")
    orig <- orig[,!colnames(orig)%in%c(colnames(kV),colnames(nV)),drop=FALSE]
    d <- orig
    if(!is.null(nV))
      d <- cbind(nV,d)
    if(!is.null(kV))
      d <- cbind(kV,d)
    xtmp <- cbind(fk[rko],rk[rko],d[rko,])
    colnames(xtmp) <- c("fk","risk",colnames(d))
    xtmp <- xtmp[order(xtmp[,2],decreasing=TRUE),]
    win = gwindow("Observations with highest risk", parent=window)
    mainGroup1 = ggroup(container=win, horizontal=FALSE)
    vkT <- gtable(data.frame(apply(xtmp,2,function(x)as.character(x)),stringsAsFactors=FALSE))
    size(vkT) <- c(800,600)
    add(mainGroup1, vkT)
  }  
  # function for button ir_button (plotIndivRisk)
  # indivRiskGroup function
  # x ... object of class indivRisk
  # y ... object of class freqCalc
  plotIndivRisk <- function(...) {
    method = "histogram"
    putd("method","histogram")
    m1 <- ActiveSdcVars("risk")
    mu <- m1$global$threshold
    rk <-  m1$individual[,1]
    fk <-  m1$individual[,2]
    if(is.na(mu))
      mu <- quantile(rk,.9, na.rm=TRUE)
    sd <- 1/length(rk) * (sum(fk[rk < mu] * rk[rk < mu]) + mu*sum(fk[rk>mu])) * 100
    s2 <- length(which(rk > mu))
    mu.old <- mu
    sd.old <- sd
    s2.old <- s2
    maxsd <- 1/length(rk) * (sum(fk * rk)) *100
    knames <- ActiveSdcVarsStr()
    n1 <- knames[1]    ## next, the plot of column names of keys
    if( length(knames) > 1 ){
      for(i in 2:length(knames)){
        n1 <- paste(n1, "x", knames[i])
      }
    }
    norm.refresh <- function(...) {
      method = getd("method")
      mu <- as.numeric(evalq(svalue(smu)))
      sd <- as.numeric(evalq(svalue(ssd)))
      s2 <- as.numeric(evalq(svalue(ss2)))
      if (mu != mu.old) {
        s2 <- round(length(which(rk > mu)))
        sd <- 1/length(rk) * (sum(fk[rk < mu] * rk[rk < mu]) + mu*sum(fk[rk>mu])) * 100
        try(svalue(ssd)<-sd)
        try(svalue(ss2)<-s2)
        sd.old <<- sd
        s2.old <<- s2
      }
      if (sd != sd.old) {
        sd <- as.numeric(evalq(tclvalue(s2)))#, envir = slider.env))
        s2 <- length(which(rk > mu))
        try(svalue(ssd)<-sd)
        try(svalue(ss2)<-s2)
        sd.old <<- sd
        s2.old <<- s2
      }
      if (s2 != s2.old) {
        s2 <- as.numeric(evalq(tclvalue(s2)))#, envir = slider.env))
        sd <- 1/length(rk) * (sum(fk * rk) + 0.02*sum(fk))
        try(svalue(ssd)<-sd)
        sd.old <<- sd
        s2.old <<- length(which(rk > mu))
      }
      if( method == "histogram" ){
        hist(rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow")
        abline(v=mu, col="blue", lwd=2)
      }
      if( method == "ecdf" ){
        plot(ecdf(rk), main="ecdf of individual risk", xlab="individual risk")
        abline(v=mu, col="blue", lwd=2)
      }
    }
    plot1 <- function(method){
      if( method == "histogram" ){
        putd("method","histogram")
        hist(rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow")
        abline(v=mu, col="blue", lwd=2)
      }
      if( method == "ecdf" ){
        putd("method","ecdf")
        plot(ecdf(rk), main="ecdf of individual risk", xlab="individual risk")
        abline(v=as.numeric(evalq(svalue(smu))), col="blue", lwd=2)
      }
    }
    win = gwindow("Individual Risk Adjustments", parent=window)
    mainGroup1 = ggroup(container=win, horizontal=FALSE)
    method = "histogram"
    sliderGroup = ggroup(container=mainGroup1, horizontal=FALSE)
    tmp = gframe("Individual Risk Threshold", container=sliderGroup)
    mustart <- round(mu/0.001)*0.001
    tostart <- round(max(rk)/0.001)*0.001+0.001
    smu = gslider(from=0, to=tostart, by=0.001, value=mustart, handler=norm.refresh)
    add(tmp, smu, expand=TRUE)
    tmp = gframe("Re-identification Rate", container=sliderGroup)
    sdstart <- round(sd/0.01)*0.01
    to2start=round(maxsd/0.01)*0.01+0.01
    ssd = gslider(from=0, to=to2start, by=0.01, value=sdstart, handler=norm.refresh)
    add(tmp, ssd, expand=TRUE)
    tmp = gframe("Unsafe Records", container=sliderGroup)
    s2start <- round(s2)
    ss2 = gslider(from=0, to=length(rk), by=1, value=s2start, handler=norm.refresh)
    add(tmp, ss2, expand=TRUE)
    gbutton("Show ecdf", container=mainGroup1, handler=function(x,...) plot1("ecdf"))
    gbutton("Show histogram", container=mainGroup1, handler=function(x,...) plot1("histogram"))
    gbutton("Suppress above threshold", container=mainGroup1, handler=function(x,...){
          smuval=as.numeric(svalue(smu))
          dispose(win)
          localSupp_tmp(threshold=smuval)
        })
    add(mainGroup1, ggraphics())
    if( method == "histogram" ){
      try(hist(rk, main=n1,freq=TRUE, xlab="individual risk", col="yellow"), silent=TRUE)
      try(abline(v=mu, col="blue", lwd=2), silent=TRUE)
    }
  }
  
  # FreqCalc and indivRisk calculation - freqCalc()
  #                                    - indivRisk()
  # TODO: not needed - save freqCalcIndivRisk for script/history
  freqCalcIndivRisk <- function(...) {
    xprogressFQ = gwindow("please wait", width=250, height=140, parent=window)
    glabel("... calculating ...", container=xprogressFQ)
    # freqCalc
    ActiveSdcObject(measure_risk(ActiveSdcObject()))
    tmp <- capture.output(printFrequenciesComp(ActiveSdcObject()))
    fc_print <- getd("fc_print")
    svalue(fc_print) <- tmp[1]
    if(existd("ffc_print")){
      ffc_print <- getd("ffc_print")
      if(isExtant(ffc_print)){
        svalue(ffc_print) <- tmp[1]
        if( length(tmp)> 1 ) {
          for( i in 2:length(tmp) ) {
            insert(ffc_print, tmp[i])
          }
        }
      } 
    }
    if( length(tmp)> 1 ) {
      for( i in 2:length(tmp) ) {
        insert(fc_print, tmp[i])
      }
    }
    #-- End - print.freqCalc
    #-- Start - summary.freqCalc
    
    tmp <- capture.output(printLocalSuppression(ActiveSdcObject()))
    svalue(fc_summary) <- tmp[1]
    if( length(tmp)> 1 ) {
      for( i in 2:length(tmp) ) {
        if( !tmp[i] == "" ) {
          insert(fc_summary, tmp[i])
        }
      }
    }
    
    tmp <- capture.output(printRecode(ActiveSdcObject()))
    svalue(recode_summary) <- tmp[1]
    if( length(tmp)> 1 ) {
      for( i in 2:length(tmp) ) {
        if( !tmp[i] == "" ) {
          insert(recode_summary, tmp[i])
        }
      }
    }
    
    #Measure Risk Funktion
    if(!is.null(ActiveSdcVars())){
      tmp <- capture.output(printMeasure_riskComp(ActiveSdcObject()))
    }else{
      tmp <- "No Risk available at the moment"
    }
    svalue(ir_print) <- tmp[1]
    if( length(tmp)> 1 ) {
      for( i in 2:length(tmp) ) {
        insert(ir_print, tmp[i])
      }
    }
    #-- End - print.indivRisk
    dispose(xprogressFQ)
  }
  
  # TODO: var to factor tmp
  varToFactor_tmp <- function(var){
    Script.add(paste("sdcObject <- varToFactor(sdcObject,var=", 
            parseVarStr(var), 
            ")", sep=""))  
    ActiveSdcObject(varToFactor(ActiveSdcObject(),var=var))
  }
  varToNumeric_tmp <- function(var){
    xtmp <- get.sdcMicroObj(ActiveSdcObject(), type="manipKeyVars")
    suppressWarnings(tmpvar <- as.numeric(as.character(xtmp[,var])))
    if(sum(is.na(tmpvar))>sum(is.na(xtmp[,var]))){
      if(existd("rb")){
        rb <- getd("rb")
        keyname <- ActiveSdcVarsStr()
        ind <- which(keyname==var)
        svalue(rb[[ind]]) <- "Factor"
        gr1_window <- getd("gr1_window")
        gmessage("Variable cannot be changed to numeric!", title="Information", icon="info", parent=gr1_window)
      }
    }else{
      ActiveSdcObject(varToNumeric(ActiveSdcObject(),var=var))
      Script.add(paste("sdcObject <- varToNumeric(sdcObject,var=", 
              parseVarStr(var), 
              ")", sep=""))
    }
#	print(head(xtmp))
    
  }
  pram_tmp <- function(var,strata_var=NULL){
    xprogress = gwindow("please wait", width=180, height=40)
    glabel("... script running ...", container=xprogress)
    seed_pram <- round(runif(1)*10e5)
    if(length(strata_var)>0){
      strata_var <- parseVarStr(strata_var)
      Script.add(paste("sdcObject <- pram_strata(sdcObject,variables=", 
              parseVarStr(var),",strata_variables=",strata_var, 
              ",seed=",seed_pram,")", sep="")) 
      ActiveSdcObject(pram_strata(ActiveSdcObject(),variables=var,strata_variables=strata_var,seed=seed_pram))
    }else{
      strata_var <- parseVarStr(strata_var)
      Script.add(paste("sdcObject <- pram_strata(sdcObject,variables=", 
              parseVarStr(var),",seed=",seed_pram,")", sep="")) 
      ActiveSdcObject(pram_strata(ActiveSdcObject(),variables=var,seed=seed_pram))
    } 
    freqCalcIndivRisk()
    dispose(xprogress)
  }
  #LocalSuppression
  localSuppression_tmp <- function(k, importance) {
    Script.add(paste("sdcObject <- localSuppression(sdcObject,k=", parseVar(k), ",importance=", parseVar(importance), ")", sep=""))
    xprogress = gwindow("please wait", width=180, height=40)
    glabel("... script running ...", container=xprogress)
    importance <- importance
    ActiveSdcObject(localSuppression(obj=ActiveSdcObject(), k=k, importance=importance))
    freqCalcIndivRisk()
    dispose(xprogress)
  }
  
  
  # microaggregation_tmp - microaggregation()
  # TODO: done - save microaggregation for script/history
  microaggregation_tmp <- function(aggr, method, vars,strata_variables=NULL) {
    xprogress = gwindow("please wait", width=180, height=40)
    glabel("... script running ...", container=xprogress)
    if(length(strata_variables)==0){
      Script.add(paste("sdcObject <- microaggregation(sdcObject,aggr=", parseVar(aggr), ", method=",
              parseVarStr(method), ", variables=", parseVarStr(vars), ")", sep=""))
      strata_variables <- NULL
    }else{
      Script.add(paste("sdcObject <- microaggregation(sdcObject,aggr=", parseVar(aggr), ", method=",
              parseVarStr(method), ", variables=", parseVarStr(vars),",strata_variables=",parseVarStr(strata_variables), ")",
              sep=""))
      
    }
    ActiveSdcObject(microaggregation(ActiveSdcObject(), method=method, aggr=aggr,variables=vars,strata_variables=strata_variables))
    freqCalcIndivRisk()
    nm_risk_print_function()
    dispose(xprogress)
  }
  localSupp_tmp <- function(threshold) {
    putd("threshold",threshold)
    nm2_window = gwindow("Suppress above threshold", width=230, parent=window,height=300)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("localSupp", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    tmp = gframe("key-Variable to supress", container=nm2_windowGroup, horizontal=FALSE)
    VarSel = gdroplist(ActiveSdcVarsStr())
    tt_var <- "For observation with risk above the threshold, this variable will be deleted."
    tooltip(VarSel) <- tt_var
    add(tmp, VarSel)
    gseparator(container=nm2_windowGroup)
    nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
    addSpring(nm2_windowButtonGroup)
    gbutton("Ok", container=nm2_windowButtonGroup,
        handler=function(h,...) {
          Var=svalue(VarSel)
          xprogress = gwindow("please wait", width=180, height=40)
          glabel("... script running ...", container=xprogress)
          Script.add(paste("sdcObject <- localSupp(sdcObject,threshold=", parseVar(getd("threshold")),"keyVar=",parseVarStr(Var),")",sep=""))
          
          ActiveSdcObject(localSupp(ActiveSdcObject(), threshold=getd("threshold"),keyVar=Var))
          freqCalcIndivRisk()
          dispose(nm2_window)
          dispose(xprogress)
          #plotIndivRisk()
        })
    gbutton("Cancel", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("microaggregation") })
    
    
    
    
    
  }
  # shuffle_tmp - shuffle()
  shuffle_tmp <- function( method,regmethod,covmethod, xvars,yvars) {
    xprogress = gwindow("please wait", width=180, height=40)
    glabel("... script running ...", container=xprogress)
    form <- paste(paste(xvars,collapse="+"),"~",paste(yvars,collapse="+"))
    Script.add(paste("sdcObject <- shuffle(sdcObject,method=", parseVarStr(method), ",regmethod= ",parseVarStr(regmethod), ", covmethod=",parseVarStr(covmethod), ", form=",
            form, ")", sep=""))
    ActiveSdcObject(shuffle(obj=ActiveSdcObject(), form=as.formula(form), method=method, regmethod=regmethod,covmethod=covmethod))
    nm_risk_print_function()
    freqCalcIndivRisk()
    dispose(xprogress)
  }
  ls4 <- function(...){
    nm2_window = gwindow("Local Suppression", width=230, parent=window,height=400)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    ls3_pars = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("localSuppression", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    tmp = gframe("k-Anonymity parameter", container=ls3_pars)
    x = gslider(2, 12, by=1)
    add(tmp, x, expand=TRUE)
    y_tmp <- get.sdcMicroObj(ActiveSdcObject(),"manipKeyVars")
    y <- list()
    xxtmp <- apply(y_tmp, 2, function(x) { length(table(x))})
    importance <- match(xxtmp, sort(xxtmp, decreasing=FALSE))
    y_tmp <- ActiveSdcVarsStr()
    for( i in 1:length(y_tmp) ) {
      fns <- eval(parse(text=paste("
                      function(...){
                      if(existd(\"impslider\")){      
                      ii <- ",i,"
                      y <- getd(\"impslider\")
                      yval <- as.numeric(as.vector(lapply(y,svalue)))
                      valtmp <- c(1:length(y))[-yval[ii]]
                      yval[-ii][order(yval[-ii])] <- valtmp
                      yval[yval[ii]<yval] <- yval[yval[ii]<yval]+1
                      for(i in 1:length(yval)){
                      svalue(y[[i]]) <- yval[i]
                      }
                      } 
                      }
                      ",sep="")))
      y[[i]] <- gslider(from=1, to=length(importance), by=1, value=importance[i],handler=fns)
    }
    putd("impslider",y)
    tmp = gframe("Importance of keyVars", container=ls3_pars, horizontal=FALSE)
    for( i in 1:length(y_tmp) ) {
      tmpg = ggroup(container=tmp)
      tmpt = glabel(y_tmp[i])
      add(tmpg, tmpt, expand=TRUE)
      add(tmpg, y[[i]], expand=TRUE)
    }
    gseparator(container=ls3_pars)
    ls3_parsButtonGroup = ggroup(container=ls3_pars)
    addSpring(ls3_parsButtonGroup)
    gbutton("Ok", container=ls3_parsButtonGroup,
        handler=function(h,...) {
          importance <- as.numeric(as.vector(lapply(y,svalue)))
          k <- svalue(x)
          localSuppression_tmp(k, importance)
          dispose(nm2_window)
#          }
        })
    gbutton("Cancel", container=ls3_parsButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help", container=ls3_parsButtonGroup, handler=function(h,...) { helpR("localSuppression") })
    
    
  }
  # function for nm_button2
  # globalRecodeGroup-numericalMethods function
  nm2 <- function(...) {
    #Tooltip Microaggegation
    tt_aggr <- "aggregation level (default=3)"
    tt_method <- "mdav, rmd, pca, clustpppca, influence"
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)" 
    tt_ltr1 <- "Add selected strata variable(s)"
    tt_rtl1 <- "Remove selected strata variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    lTOr1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab1[])==1 ) {
          if( is.na(selTab1[]) ) {
            selTab1[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(sTab[]) ) {
          sTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(sTab[]) ) {
            for( j in 1:length(h) ) {
              if( sTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          sTab[,] <- data.frame(vars=sTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(sTab[])==1 ) {
          if( is.na(sTab[]) ) {
            sTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab1[]) ) {
          selTab1[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab1[]) ) {
            for( j in 1:length(h) ) {
              if( selTab1[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab1[,] <- data.frame(vars=selTab1[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    
    nm2_window = gwindow("Microaggregation", width=230, parent=window,height=600)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("microaggregation", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    tmp = gframe("Aggregation level", container=nm2_windowGroup, horizontal=FALSE)
    ntmp = ggroup(container=tmp)
    aggrSel = gslider(from=2, to=20, by=1)
    tooltip(aggrSel) <- tt_aggr
    svalue(aggrSel) <- 3
    add(ntmp, aggrSel, expand=TRUE)
    tmp = gframe("Method", container=nm2_windowGroup, horizontal=FALSE)
    methodSel = gdroplist(c("mdav","rmd", "pca", "clustpppca", "influence"))
    tooltip(methodSel) <- tt_method
    add(tmp, methodSel)
    tmp = gframe("Variable selection", container=nm2_windowGroup)
    numVars <- c()
    # just use all numerical vars
    #for( i in 1:dim(xtmp)[2] ) {
    #	if( is.numeric(xtmp[,i]) & names(xtmp)[i] != ActiveSdcVarsStr("weightVar") ) {
    #		numVars <- c(numVars, names(xtmp)[i])
    #	}
    #}
    numVars <- ActiveSdcVarsStr("numVars")
    varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    
    
    tmp = gframe("Strata Variable selection", container=nm2_windowGroup)
    sVars <- c()
    # just use all numerical vars
    #for( i in 1:dim(xtmp)[2] ) {
    #	if( is.numeric(xtmp[,i]) & names(xtmp)[i] != ActiveSdcVarsStr("weightVar") ) {
    #		numVars <- c(numVars, names(xtmp)[i])
    #	}
    #}
    sVars <- ActiveSdcVarsStr("strataVar")
    keyVars <- ActiveSdcVarsStr()
    sTab = gtable(data.frame(vars=c(sVars,keyVars), stringsAsFactors=FALSE), multiple=TRUE)
    size(sTab) <- c(120,200)
    add(tmp, sTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr1(svalue(sTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl1(svalue(selTab1)) })
    tooltip(b1) <- tt_ltr1
    tooltip(b2) <- tt_rtl1
    addSpring(btmp)
    selTab1 = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab1) <- c(120,200)
    add(tmp, selTab1)
    
    
    gseparator(container=nm2_windowGroup)
    nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
    addSpring(nm2_windowButtonGroup)
    gbutton("Ok", container=nm2_windowButtonGroup,
        handler=function(h,...) {
          aggrVal <- as.numeric(svalue(aggrSel))
          if( length(selTab[])<1 | any(is.na(selTab[])) ) {
            gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=nm2_window)
          } else {
            microaggregation_tmp(aggrVal, svalue(methodSel), vars=selTab[],strata_variables=selTab1[])
            dispose(nm2_window)
          }
        })
    gbutton("Cancel", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("microaggregation") })
  }
  ldiv1 <- function(...) {
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)" 
    tt_slider1 <- "l_recurs_c Parameter"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    
    nm2_window = gwindow("l-diversity", width=230, parent=window,height=600)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("measure_risk", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    tmp = gframe("l Recursive Constant", container=nm2_windowGroup, horizontal=FALSE)
    recconst = gslider(from=1, to=10, by=1, value=2)
    tooltip(recconst) <- tt_slider1
    enabled(recconst) = TRUE
    add(tmp, recconst, expand=TRUE)
    
    tmp = gframe("Choose sensitive variable(s)", container=nm2_windowGroup, horizontal=FALSE)
    
    xtmp <- ActiveSdcObject()@origData
    numVars <- ActiveSdcVarsStr("numVars")
    keyVars <- ActiveSdcVarsStr()
    hVars <- ActiveSdcVarsStr("hhId")
    wVars <- ActiveSdcVarsStr("weightVar")
    sVars <- ActiveSdcVarsStr("strataVar")
    posssensVars <- colnames(xtmp)[!colnames(xtmp)%in%c(numVars,keyVars,hVars,wVars,sVars)]
    
    varTab = gtable(data.frame(vars=posssensVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=TRUE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    
    gseparator(container=nm2_windowGroup)
    nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
    addSpring(nm2_windowButtonGroup)
    gbutton("Ok", container=nm2_windowButtonGroup,
        handler=function(h,...) {
          if( length(selTab[])<1 | any(is.na(selTab[])) ) {
            gmessage("You need to select at least 1 variables!", title="Information", icon="info", parent=nm2_window)
          } else {
            ActiveSdcObject(ldiversity(ActiveSdcObject(),ldiv_index=selTab[],l_recurs_c=svalue(recconst)))
            dispose(nm2_window)
            ldiverg_window = gwindow("l-diversity", width=520, parent=window,height=400)
            nb <- gnotebook(container=ldiverg_window, closebuttons=FALSE)
            #Main
            nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
            #Help
            t <- gtext(container=nb, label="Help", expand=TRUE)
            l <- .findHelpPage("ldiversity", "sdcMicro")
            x <- l$x
            .insertHelpPage(t, x)
            svalue(nb) <- 1
            tmp = gframe("Output", container=nm2_windowGroup, horizontal=FALSE)
            gte <- gtext("", container=tmp, height=250, width=500)
            vk_button = gbutton("View Observations violating 2 l-diversity", container=tmp,
                handler=function(h, ...) viewldiv())
            
            
            svalue(gte) <- capture.output(print(ActiveSdcObject()@risk$ldiversity),append=FALSE)
            gseparator(container=nm2_windowGroup)
            nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
            addSpring(nm2_windowButtonGroup)
            gbutton("Ok", container=nm2_windowButtonGroup,handler=function(h,...)dispose(ldiverg_window))
            gbutton("Help", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("ldiversity") })
          }
        })
    gbutton("Cancel", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("ldiversity") })
  }
  
  # addNoise_tmp - addNoise()
  # TODO: done - save addNoise for script/history
  addNoise_tmp <- function(noise, method, vars) {
    xprogress = gwindow("please wait", width=180, height=40)
    glabel("... script running ...", container=xprogress)
    Script.add(paste("sdcObject <- addNoise(sdcObject,noise=", parseVar(noise), ",method= ",
            parseVarStr(method), ",variables= ", parseVarStr(vars), ")", sep=""))
    ActiveSdcObject(addNoise(ActiveSdcObject(),noise=noise,method=method,variables=vars))
    freqCalcIndivRisk()
    nm_risk_print_function()
    dispose(xprogress)
  }
  
  # function for nm_button1
  nm1 <- function(...) {
    #ToolTip Addnoise Window
    tt_noise <- "amount of noise (in percentages)"
    tt_method <- "choose between additive and correlated2"
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    nm1_window = gwindow("Add noise", width=230, parent=window)
    nb <- gnotebook(container=nm1_window, closebuttons=FALSE)
    #Main
    nm1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    tmp = gframe("Noise", container=nm1_windowGroup, horizontal=FALSE)
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("addNoise", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    ntmp = ggroup(container=tmp)
    glabel("Value between 0 and 2000", container=ntmp)
    noiseSel = gedit()
    svalue(noiseSel) <- "150"
    tooltip(noiseSel) <- tt_noise
    add(ntmp, noiseSel)
    tmp = gframe("Method", container=nm1_windowGroup, horizontal=FALSE)
    methodSel = gdroplist(c("correlated2","additive"))
    tooltip(methodSel) <- tt_method
    add(tmp, methodSel)
    tmp = gframe("Variable selection", container=nm1_windowGroup)
    numVars <- c()
    # not all vars, just numerical vars
    #for( i in 1:dim(xtmp)[2] ) {
    #	if( class(xtmp[,i])=="numeric" & names(xtmp)[i] != ActiveSdcVarsStr("weightVar") ) {
    #		numVars <- c(numVars, names(xtmp)[i])
    #	}
    #}
    numVars <- ActiveSdcVarsStr("numVars")
    varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    gseparator(container=nm1_windowGroup)
    nm1_windowButtonGroup = ggroup(container=nm1_windowGroup)
    addSpring(nm1_windowButtonGroup)
    gbutton("Ok", container=nm1_windowButtonGroup,
        handler=function(h,...) {
          noise <- as.numeric(svalue(noiseSel))
          if( !is.numeric(noise) | is.na(noise) ) {
            gmessage("Noise needs to be a numeric value!", title="Information", icon="info", parent=nm1_window)
          } else {
            if( length(selTab[])==0 | any(is.na(selTab[])) ) {
              gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=nm1_window)
            } else {
              addNoise_tmp(noise, svalue(methodSel), selTab[])
              dispose(nm1_window)
            } 
          }
        })
    gbutton("Cancel", container=nm1_windowButtonGroup, handler=function(h,...) { dispose(nm1_window) })
    gbutton("Help", container=nm1_windowButtonGroup, handler=function(h,...) { helpR("addNoise") })
  }
  
  # function for shuffle_button1
  shuffle1 <- function(...) {
    #Tooltip SHUFFLE
    tt_method <- "mdav, rmd, pca, clustpppca, influence"
    tt_regmethod <- "lm, MM"
    tt_covmethod <- c("spearman, pearson, mcd")
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)" 
    tt_ltr1 <- "Add selected strata variable(s)"
    tt_rtl1 <- "Remove selected strata variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
        if(any(selTab[,]%in%sTab[,])){
          sTab[,] <- sTab[,][-which(sTab[,]%in%selTab[,])]
        }
        if(any(selTab[,]%in%selTab1[,])){
          selTab1[,] <- selTab1[,][-which(selTab1[,]%in%selTab[,])]
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
        sTab[,] <- data.frame(vars=c(sTab[,], h), stringsAsFactors=FALSE)
      }
    }
    lTOr1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab1[])==1 ) {
          if( is.na(selTab1[]) ) {
            selTab1[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(sTab[]) ) {
          sTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(sTab[]) ) {
            for( j in 1:length(h) ) {
              if( sTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          sTab[,] <- data.frame(vars=sTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(sTab[])==1 ) {
          if( is.na(sTab[]) ) {
            sTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab1[]) ) {
          selTab1[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab1[]) ) {
            for( j in 1:length(h) ) {
              if( selTab1[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab1[,] <- data.frame(vars=selTab1[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    
    nm2_window = gwindow("Shuffling", width=230, parent=window,height=600)
    nb <- gnotebook(container=nm2_window, closebuttons=FALSE)
    #Main
    nm2_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("shuffle", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    tmp = gframe("Shuffling Method", container=nm2_windowGroup, horizontal=FALSE)
    methodSel = gdroplist(c("ds","mvn", "mlm"))
    tooltip(methodSel) <- tt_method
    add(tmp, methodSel)
    tmp = gframe("Regression Method", container=nm2_windowGroup, horizontal=FALSE)
    regmethodSel = gdroplist(c("lm","MM"))
    tooltip(regmethodSel) <- tt_regmethod
    add(tmp, regmethodSel)
    tmp = gframe("Covariance Method", container=nm2_windowGroup, horizontal=FALSE)
    covmethodSel = gdroplist(c("spearman","pearson","mcd"))
    tooltip(covmethodSel) <- tt_covmethod
    add(tmp, covmethodSel)
    
    
    tmp = gframe("Numerical variable selection (Responses)", container=nm2_windowGroup)
    numVars <- c()
    numVars <- ActiveSdcVarsStr("numVars")
    varTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    
    
    tmp = gframe("Variable selection (Predictors)", container=nm2_windowGroup)
    xtmp <- ActiveDataSet()
    sVars <- colnames(xtmp)
    sTab = gtable(data.frame(vars=c(sVars), stringsAsFactors=FALSE), multiple=TRUE)
    size(sTab) <- c(120,200)
    add(tmp, sTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr1(svalue(sTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl1(svalue(selTab1)) })
    tooltip(b1) <- tt_ltr1
    tooltip(b2) <- tt_rtl1
    addSpring(btmp)
    selTab1 = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab1) <- c(120,200)
    add(tmp, selTab1)
    
    
    gseparator(container=nm2_windowGroup)
    nm2_windowButtonGroup = ggroup(container=nm2_windowGroup)
    addSpring(nm2_windowButtonGroup)
    gbutton("Ok", container=nm2_windowButtonGroup,
        handler=function(h,...) {
          if( length(selTab[])<1 | any(is.na(selTab[])) ) {
            gmessage("You need to select at least 1 numeric variable!", title="Information", icon="info", parent=nm2_window)
          }else if( length(selTab1[])<2 | any(is.na(selTab1[])) ) {
            gmessage("You need to select at least 2 predictor variable!", title="Information", icon="info", parent=nm2_window)
          } else {
            shuffle_tmp(method=svalue(methodSel),regmethod=svalue(regmethodSel),covmethod=svalue(covmethodSel), xvars=selTab[],yvars=selTab1[])
            dispose(nm2_window)
          }
        })
    gbutton("Cancel", container=nm2_windowButtonGroup, handler=function(h,...) { dispose(nm2_window) })
    gbutton("Help", container=nm2_windowButtonGroup, handler=function(h,...) { helpR("shuffle") })
    
  }
  
  # needed sub functions
  # TODO: done - save rename for script/history
  renameVars_tmp <- function(v, h, newName, redo=FALSE) {
    if( !redo ) {
      Script.add(paste("sdcObject <- renameVars(sdcObject,var=", parseVarStr(v), ", before=",
              parseVarStr(h), ", after=", parseVarStr(newName), ")", sep=""))
    }
    ActiveSdcObject(renameVars(ActiveSdcObject(),var=v,before=h,after=newName))
  }
  # TODO: done - save group for script/history
  groupVars_tmp <- function(v, h, newName, redo=FALSE) {
    if( !redo ) {
      Script.add(paste("sdcObject <- groupVars(sdcObject,var=", parseVarStr(v), ", before=",
              parseVarStr(h), ", after=", parseVarStr(newName), ")", sep=""))
    }
    ActiveSdcObject(groupVars(ActiveSdcObject(),var=v,before=h,after=newName))
  }
  # group and rename variables
  # globalRecodeGroup function
  
  # globalRecode_tmp - globalRecode()
  # TODO: replace cut with globalRecode as soon as it is corrected
  # TODO: done - save globalRecode for script/history
  globalRecode_tmp <- function(var, breaks, labels, redo=FALSE) {
    if(is.logical(labels))
      labels <- NULL
    if( !redo ) {
      Script.add(paste("sdcObject <- globalRecode(sdcObject,column=", parseVarStr(var), ", breaks=",
              parseVar(breaks), ", labels=", parseVarStr(labels), ")", sep=""))
    }
    ActiveSdcObject(globalRecode(ActiveSdcObject(),column=var,breaks=breaks,labels=labels))
    freqCalcIndivRisk()
  }
  
  # globalRecodeGroup function
  vc <- function(...) {
    renameFacVar <- function(h, v, ...) {
      gr1_window <- getd("gr1_window")
      if( length(h)< 1 ) {
        gmessage("You need to select at least 1 level.", title="Information", icon="warning")
      } else {
        if( length(h)> 1 ) {
          gmessage("To rename one, you just have to select 1.", title="Information",
              icon="warning", parent=gr1_window)
        } else {
          newName <- ginput("Please enter a new level name.", parent=gr1_window)
          if( !is.na(newName) & newName!="" ) {
            renameVars_tmp(v, h, newName)
            #cat("v:\n")
            #print(v)
            #cat("h:\n")
            #print(h)
            showLevels(v)
            updateSummary(v)
          }
        }
      }
    }
    groupFacVar <- function(h, v, ...) {
      gr1_window <- getd("gr1_window")
      if( length(h)< 2 ) {
        gmessage("You need to select at least 2 levels to group.", title="Information",
            icon="warning", parent=gr1_window)
      } else {
        levName <- h[1]
        for( i in 2:length(h) ) {
          levName <- paste(levName, ";", h[i], sep="")
        }
        newName <- ginput("Please enter a new level name.", text=levName, parent=gr1_window)
        if( !is.na(newName) ) {
          groupVars_tmp(v, h, newName)
          showLevels(v)
          updateSummary(v)
        }
      }
    }
    updateSummary <- function(v){
      index <- which(ActiveSdcVarsStr()==v)
      if(existd("SummaryTab")){
        #gr1_head <- getd("gr1_head")
        #gr1_summary <- getd("gr1_summary")
        SummaryTab <- getd("SummaryTab")
        xtmp <- ActiveSdcObject()@manipKeyVars
        var <- xtmp[,v]
        if(isExtant(SummaryTab[[index]])){
          #svalue(gr1_head[[index]]) <- capture.output(print(head(var)),append=FALSE)
          Supdate <- t(as.data.frame(table(var)))
          SummaryTab[[index]][,1:ncol(Supdate)] <- Supdate
          if(ncol(SummaryTab[[index]][,])>ncol(Supdate)){
            ind <- (ncol(Supdate)+1):ncol(SummaryTab[[index]][,])
            SummaryTab[[index]][1,ind] <- rep("",length(ind))
            SummaryTab[[index]][2,ind] <- rep("",length(ind))
            names(SummaryTab[[index]])[ind] <- rep("x_x",length(ind ))
          }
          #names(SummaryTab[[index]]) <- colnames(dd_summary)
          #svalue(gr1_summary[[index]]) <- capture.output(print(summary(var)),append=FALSE)
        }
        dev.set(getd("gdev")[[index]])
        if(is.factor(var)){
          try(plot(var,main=v),silent=TRUE)
        }else if(is.numeric(var)){
          try(hist(var,main=v),silent=TRUE)
        }
        keyname <- ActiveSdcVarsStr()
        varmoslist <- keyname[unlist(lapply(keyname,function(x)is.factor(xtmp[,x])))]
        mosdev <- getd("mosdev")
        dev.set(mosdev)
        if(length(varmoslist)>=2){
          formX <- varmoslist
          try(mosaic_check(formX),silent=TRUE)
        }else{
          try(plot(1,main="Two variables as factors needed!"),silent=TRUE)
        }
        FreqT <- getd("FreqT")
        if(isExtant(FreqT)){
          m1 <- ActiveSdcVars("risk")$individual
          m1[,"risk"] <- round(m1[,"risk"],5)
          xtmp <- ActiveSdcObject()@manipKeyVars
          tabDat <- cbind(xtmp[,keyname,drop=FALSE],m1)
          ind <- !duplicated(apply(xtmp[,keyname,drop=FALSE],1,function(x)paste(x,collapse="_")))
          tabDat <- tabDat[ind,]
          tabDat <- tabDat[order(as.numeric(tabDat$risk),decreasing=TRUE),]
          tabDat <- apply(tabDat,2,function(x)as.character(x))
          FreqT[,] <- data.frame(tabDat,stringsAsFactors=FALSE)
        }
      }
      freqCalcIndivRisk()
    }
    showLevels <- function(h, ...) {
      if(existd("facTab")){
        facTab <- getd("facTab")
        i <- which(ActiveSdcVarsStr()==h)
        x <- facTab[[i]]
        if(isExtant(x)){
          xtmp <- ActiveSdcObject()@manipKeyVars
          x[,] <- levels(xtmp[,h])
          gr3_windowButton1 <- getd("gr3_windowButton1")
          gr3_windowButton2 <- getd("gr3_windowButton2")
          enabled(gr3_windowButton1[[i]]) <- TRUE
          enabled(gr3_windowButton2[[i]]) <- TRUE
        }
      }
    }
    hideLevels <- function(h, ...) {
      facTab <- getd("facTab")
      i <- which(ActiveSdcVarsStr()==h)
      x <- facTab[[i]] 
      x[,] <- character(0)
      gr3_windowButton1 <- getd("gr3_windowButton1")
      gr3_windowButton2 <- getd("gr3_windowButton2")
      enabled(gr3_windowButton1[[i]]) <- FALSE
      enabled(gr3_windowButton2[[i]]) <- FALSE
    }
    keyname <- ActiveSdcVarsStr()
    gr1_window = gwindow("Choose parameters for globalRecode", width=1100, parent=window)
    gr1_main <-  gframe("", container=gr1_window, horizontal=FALSE)
    nb <- gnotebook(container=gr1_main, closebuttons=FALSE)
    #Main
    xtmp <- ActiveSdcObject()@manipKeyVars
    groupFacVarFun <- renameFacVarFun <- gdev <- recFactorFun <- breaksInput <- labelsInput <- list()
    #facTab <- gr3_windowButton1 <- gr3_windowButton2 <- recButton2 <- rb <- gr1_head <- gr1_summary <- rbfun <- list()
    facTab <- gr3_windowButton1 <- gr3_windowButton2 <- recButton2 <- rb <- SummaryTab <- rbfun <- list()
    for(i in 1:length(keyname)){
      #Main
      tmp <- ggroup(horizontal=FALSE, container=nb,label=keyname[i]) 
      glabel("Type:",container=tmp)
      rb[[i]] <- gradio(c("Numeric","Factor"), container=tmp)
      rbfun[[i]] <- eval(parse(text=paste("
                      function(h,...) {
                      index <- ",i,"
                      name <- \"",keyname[i],"\"
                      if(svalue(h$obj)==\"Factor\"){
                      enabled(recButton2[[index]]) <- FALSE
                      varToFactor_tmp(name)
                      showLevels(name)
                      }else{
                      varToNumeric_tmp(name)
                      enabled(recButton2[[index]]) <- TRUE
                      hideLevels(name)
                      }
                      var <- ActiveSdcObject()@manipKeyVars[,name]
                      updateSummary(name)
                      }",sep="")))
      
      addHandlerClicked(rb[[i]], handler=rbfun[[i]])
      
      #glabel("Head:",container=tmp)
      #gr1_head[[i]] <- gtext("", container=tmp, height=50, width=250)
      glabel("Frequencies:",container=tmp)
      dd_summary <- t(as.data.frame(table(xtmp[,keyname[i]])))
      colnames(dd_summary)<- as.character(dd_summary[1,])
      dd_summary <- dd_summary[-1,,drop=FALSE]
      Supdate <- t(as.data.frame(table(xtmp[,keyname[i]])))
      colnames(Supdate) <- paste("Cat",1:ncol(Supdate),sep="")
      SummaryTab[[i]] <- gtable(Supdate)
      size(SummaryTab[[i]]) <- c(800,100)
      add(tmp, SummaryTab[[i]])
      #putd("SummaryTab",SummaryTab[[i]])
      
      #gr1_summary[[i]] <- gtext("", container=tmp, height=50, width=250)
      #svalue(gr1_summary[[i]]) <- capture.output(print(summary(xtmp[,keyname[i]])),append=FALSE)
      
      #svalue(gr1_head[[i]]) <- capture.output(print(head(xtmp[,keyname[i]])),append=FALSE)
      
      tmp2 <- gframe("", container=tmp, horizontal=TRUE)
      #####Recode to Factor
      tmpRecFac <-  gframe("Recode to factor", container=tmp2, horizontal=FALSE)
      recFactorFun[[i]] <- eval(parse(text=paste(
                  "function(...){
                      index <- ",i,"
                      name <- \"",keyname[i],'"
                      breaksInput <- getd("breaksInput")
                      labelsInput <- getd("labelsInput")
                      breaks=svalue(breaksInput[[index]])
                      labels=svalue(labelsInput[[index]])
                      breaks <- strsplit(breaks, ",")[[1]]
                      labels <- strsplit(labels, ",")[[1]]
                      allNumeric <- TRUE
                      labelsNumeric <- TRUE
                      gr_do <- TRUE
                      if( length(breaks)==0 ) {
                      allNumeric <- FALSE
                      } else {
                      try(breaks <- as.numeric(breaks), silent=TRUE)
                      for( i in 1:length(breaks) ) {
                      if( is.na(breaks[i]) ) {
                      allNumeric <- FALSE
                      }
                      }
                      }
                      if( allNumeric==FALSE ) {
                      gmessage("Breaks argument is not valid", title="Information", icon="info", parent=gr1_window)
                      gr_do <- FALSE
                      }
                      if( allNumeric ) {
                      if( length(labels)>0 ) {
                      if( length(breaks)==1 ) {
                      if( length(labels)!=breaks) {
                      gmessage(paste("Too many or few labels supplied. ",breaks," labels should be supplied.",sep=""), title="Information", icon="info", parent=gr1_window)
                      gr_do <- FALSE
                      }
                      }
                      if( length(breaks)>1 ) {
                      if( length(labels)!=(length(breaks)-1) ) {
                      gmessage(paste("Too many or few labels supplied. ",(length(breaks)-1)," labels should be supplied.",sep=""), title="Information", icon="info", parent=gr1_window)
                      gr_do <- FALSE
                      }
                      }
                      if( gr_do ) {
                      try(tmp_labels <- as.numeric(labels), silent=TRUE)
                      for( i in 1:length(tmp_labels) ) {
                      if( is.na(tmp_labels[i]) ) {
                      labelsNumeric <- FALSE
                      }
                      }
                      if( labelsNumeric ) {
                      labels <- as.numeric(labels)
                      }
                      if( !labelsNumeric ) {
                      gr_do <- gconfirm("Variable will be of typ factor afterwards", title="Information",
                      icon="warning", parent=gr1_window)
                      }
                      }
                      } else {
                      labels <- FALSE
                      }
                      }        
                      if( gr_do ) {
                      globalRecode_tmp (name, breaks, labels)     
                      #var <- ActiveDataSet()[,name]
                      rb <- getd("rb")
                      svalue(rb[[index]]) <- "Factor"
                      updateSummary(name)
                      }
                      }',sep="")))
      recButton2[[i]] <- gbutton("Recode to factor", container=tmpRecFac, handler=recFactorFun[[i]])
      lab <- "BREAKS: Example input: 1,3,5,9 splits var in 3 groups"
      lab <- paste(lab, "\n(1,3],(3,5] and (5,9]. If you just supply")
      lab <- paste(lab, "\n1 number, like 3, the var will be split in")
      lab <- paste(lab, "\n3 equal sized groups.")
      glabel(lab, container=tmpRecFac)
      breaksInput[[i]] = gedit(width=40)
      add(tmpRecFac, breaksInput[[i]], expand=TRUE)
      lab <- "LABELS: Labels are depending on your breaks-input."
      lab <- paste(lab, "\nExample inupt with breaks=1,3,5,9 or breaks=3:")
      lab <- paste(lab, "\n- leave it blank: auto numbering from 1 to 3")
      lab <- paste(lab, "\n- a,b,c: the 3 groups are named a, b and c")
      glabel(lab, container=tmpRecFac)
      labelsInput[[i]] = gedit()
      add(tmpRecFac, labelsInput[[i]] , expand=TRUE)
      
      
      gseparator(container=tmp)
      ##Group/Rename Factor
      tmpGroupFac <-  gframe("Group a factor", container=tmp2, horizontal=FALSE)
      tmpGroupFac2 = gframe("Levels", container=tmpGroupFac)
      facTab[[i]] <-  gtable(data.frame(levels=character(0), stringsAsFactors=FALSE),
          multiple=TRUE)
      size(facTab[[i]]) <- c(120,400)
      add(tmpGroupFac2, facTab[[i]])
      btmp = ggroup(container=tmpGroupFac2, horizontal=FALSE, expand=TRUE)
      renameFacVarFun[[i]] <- eval(parse(text=paste('
                      function(h,...){
                      facTab <- getd("facTab")
                      renameFacVar(svalue(facTab[[',i,']]), "',keyname[i],'")
                      }
                      ',sep="")))
      
      
      gr3_windowButton1[[i]] <- gbutton("rename",
          handler= renameFacVarFun[[i]])
      enabled(gr3_windowButton1[[i]]) <- FALSE
      groupFacVarFun[[i]] <- eval(parse(text=paste('
                      function(h,...) {
                      facTab <- getd("facTab")
                      groupFacVar(svalue(facTab[[',i,']]), "',keyname[i],'") 
                      }
                      ',sep="")))
      
      gr3_windowButton2[[i]] <-  gbutton("group",
          handler=groupFacVarFun[[i]])
      enabled(gr3_windowButton2[[i]]) <- FALSE
      add(btmp, gr3_windowButton1[[i]])
      add(btmp, gr3_windowButton2[[i]])
      gseparator(container=tmpGroupFac)
      gr3_windowButtonGroup = ggroup(container=tmpGroupFac)
      addSpring(gr3_windowButtonGroup)
      #Graphics Fenser
      tmpGraph <-  gframe("Plot", container=tmp2, horizontal=FALSE)
      ggraphics(container=tmpGraph)
      gdev[[i]] <- dev.cur()
      
      ##Main
      if(is.factor(xtmp[,keyname[i]])){
        svalue(rb[[i]]) <- "Factor"
      }else{
        svalue(rb[[i]]) <- "Numeric"
      }
    }
    #Save Input-Fields to the env
    putd("breaksInput",breaksInput)
    putd("labelsInput",labelsInput)
    putd("rb",rb)
    putd("gr1_window",gr1_window)
    putd("facTab",facTab)    
    putd("gr3_windowButton1",gr3_windowButton1)
    putd("gr3_windowButton2",gr3_windowButton2)
    #putd("gr1_head",gr1_head)
    #putd("gr1_summary",gr1_summary)
    putd("SummaryTab",SummaryTab)
    putd("gdev",gdev)
    
    #Insert Levels in List for Factor variables
    for(i in 1:length(keyname)){
      if(is.factor(xtmp[,keyname[i]])){
        showLevels(keyname[i])
      }
    }
    
    ##Mosaic Plot
    t <- ggraphics(container=nb, label="Mosaic Plot")
    mosdev <- dev.cur()
    putd("mosdev",mosdev)
    varmoslist <- keyname[unlist(lapply(keyname,function(x)is.factor(xtmp[,x])))]
    if(length(varmoslist)>=2){
      formX <- varmoslist
      try(mosaic_check(formX),silent=TRUE)
    }else{
      try(plot(1,main="Two variables as factors needed!"),silent=TRUE)
    }
    #Frequencies Tab
    FreqTT <- ggroup(horizontal=FALSE, container=nb,label="Frequencies")
    FreqTT_1 <- gframe("Information on observations violating 3-anonymity",container=FreqTT)
    ffc_print = gtext(container=FreqTT_1,text="", width=900, height=150)
    putd("ffc_print",ffc_print)
    svalue(ffc_print) <- svalue(fc_print)
    m1 <- ActiveSdcVars("risk")$individual
    xtmp <- ActiveSdcVars("manipKeyVars")
    tabDat <- cbind(xtmp,m1)
    ind <- !duplicated(apply(xtmp,1,function(x)paste(x,collapse="_")))
    tabDat <- tabDat[ind,]
    tabDat$risk <- round(tabDat$risk,5)
    tabDat <- tabDat[order(as.numeric(tabDat$risk),decreasing=TRUE),]    
    FreqT <- gtable(data.frame(apply(tabDat,2,function(x)as.character(x)),stringsAsFactors=FALSE))
    size(FreqT) <- c(900,500)
    FreqTT_2 <- gframe("Frequencies for combinations of cat. key variables",container=FreqTT)
    add(FreqTT_2 , FreqT)
    putd("FreqT",FreqT)
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("globalRecode", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    # First Keyvar-Tab
    svalue(nb) <- 1
    gseparator(container=gr1_main)
    okCancelGroup = ggroup(container=gr1_main)
    addSpring(okCancelGroup)
    gbutton("Ok", container=okCancelGroup,
        handler=function(h,...) {dispose(gr1_window) } )
    #gbutton("Cancel", container=okCancelGroup, handler=function(h,...) dispose(gr1_window) )
    gbutton("Help", container=okCancelGroup, handler=function(h,...) helpR("globalRecode") )
    
    #Plot ausfuehren
    for(i in 1:length(keyname)){
      dev.set(gdev[[i]])
      var <- xtmp[,keyname[i]]
      if(is.factor(var)){
        plot(var,main=keyname[i])
      }else if(is.numeric(var)){
        hist(var,main=keyname[i])
      }
    }
  }
  
  removeDirectID_tmp <- function(var){
    cmd <- paste("sdcObject <- removeDirectID(sdcObject,var=",parseVarStr(var),")",sep="")
    Script.add(cmd)
    ActiveSdcObject(removeDirectID(ActiveSdcObject(),var))
  }
  removeDirectID_menu<- function(...) {
    tt_var <- "choose variables to be removed"
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    p1_window = gwindow("Remove direct identifiers", width=230, parent=window)
    nb <- gnotebook(container=p1_window, closebuttons=FALSE)
    #Main
    p1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("removeDirectID", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    
    tmp = gframe("Select variables to be removed from data set", container=p1_windowGroup)
    ###Select categorical variables
    keyVars <- ActiveSdcVarsStr("keyVars")
    numVars <- ActiveSdcVarsStr("numVars")
    wVars <- ActiveSdcVarsStr("weightVar")
    sVars <- ActiveSdcVarsStr("strataVar")
    hVars <- ActiveSdcVarsStr("hhId")
    o <- ActiveSdcObject()@origData
    allVars <- colnames(o)
    allVars <- allVars[!allVars%in%c(keyVars,numVars,wVars,sVars,hVars)]
    allVars <- allVars[apply(o[,allVars],2,function(x)!all(is.na(x)))]
    varTab = gtable(data.frame(vars=allVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    gseparator(container=p1_windowGroup)
    
    
    p1_windowButtonGroup = ggroup(container=p1_windowGroup)
    addSpring(p1_windowButtonGroup)
    gbutton("Ok", container=p1_windowButtonGroup,
        handler=function(h,...) {
          if( length(selTab[])==0 ) {
            gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=p1_window)
          } else {
            var <- selTab[]
            removeDirectID_tmp(var)
            
            dispose(p1_window)
          } 
        })
    gbutton("Cancel", container=p1_windowButtonGroup, handler=function(h,...) { dispose(p1_window) })
    gbutton("Help", container=p1_windowButtonGroup, handler=function(h,...) { helpR("removeDirectID") })
  } 
  
  
  pram1 <- function(...) {
    #ToolTip Pram Window
    tt_var <- "choose categorical variables for Pram"
    tt_strat <- "choose variables for stratification"
    tt_ltr <- "Add selected variable(s)"
    tt_rtl <- "Remove selected variable(s)"
    lTOr <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab[])==1 ) {
          if( is.na(selTab[]) ) {
            selTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab[,] <- data.frame(vars=c(selTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(varTab[]) ) {
          varTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(varTab[]) ) {
            for( j in 1:length(h) ) {
              if( varTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          varTab[,] <- data.frame(vars=varTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(varTab[])==1 ) {
          if( is.na(varTab[]) ) {
            varTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          varTab[,] <- data.frame(vars=c(varTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab[]) ) {
          selTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab[]) ) {
            for( j in 1:length(h) ) {
              if( selTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    lTOr1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(selTab1[])==1 ) {
          if( is.na(selTab1[]) ) {
            selTab1[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
          }
        } else {
          selTab1[,] <- data.frame(vars=c(selTab1[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(sTab[]) ) {
          sTab[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(sTab[]) ) {
            for( j in 1:length(h) ) {
              if( sTab[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          sTab[,] <- data.frame(vars=sTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    rTOl1 <- function(h, ...) {
      if( length(h)>0 ) {
        if( length(sTab[])==1 ) {
          if( is.na(sTab[]) ) {
            sTab[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
          }
        } else {
          sTab[,] <- data.frame(vars=c(sTab[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(selTab1[]) ) {
          selTab1[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(selTab1[]) ) {
            for( j in 1:length(h) ) {
              if( selTab1[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          selTab[,] <- data.frame(vars=selTab[-xtmp], stringsAsFactors=FALSE)
        }
      }
    }
    p1_window = gwindow("Pram", width=230, parent=window)
    nb <- gnotebook(container=p1_window, closebuttons=FALSE)
    #Main
    p1_windowGroup = ggroup(container=nb, horizontal=FALSE,label="Function")
    tmp = gframe("Pram", container=p1_windowGroup, horizontal=FALSE)
    #Help
    t <- gtext(container=nb, label="Help", expand=TRUE)
    l <- .findHelpPage("pram_strata", "sdcMicro")
    x <- l$x
    .insertHelpPage(t, x)
    svalue(nb) <- 1
    
    
    tmp = gframe("Variable Selection", container=p1_windowGroup)
    ###Select categorical variables
    keyVars <- ActiveSdcVarsStr()
    varTab = gtable(data.frame(vars=keyVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,200)
    add(tmp, varTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr(svalue(varTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl(svalue(selTab)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab) <- c(120,200)
    add(tmp, selTab)
    gseparator(container=p1_windowGroup)
    
    
    #Select strata_variables
    tmp = gframe("Strata Variable Selection", container=p1_windowGroup)
    sVars <- ActiveSdcVarsStr("strataVar")
    sTab = gtable(data.frame(vars=sVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(sTab) <- c(120,200)
    add(tmp, sTab)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    b1 <- gbutton(">>", container=btmp, handler=function(h,...) { lTOr1(svalue(sTab)) })
    b2 <- gbutton("<<", container=btmp, handler=function(h,...) { rTOl1(svalue(selTab1)) })
    tooltip(b1) <- tt_ltr
    tooltip(b2) <- tt_rtl
    addSpring(btmp)
    selTab1 = gtable(data.frame(vars=character(0), stringsAsFactors=FALSE), multiple=TRUE)
    size(selTab1) <- c(120,200)
    add(tmp, selTab1)
    gseparator(container=p1_windowGroup)
    
    p1_windowButtonGroup = ggroup(container=p1_windowGroup)
    addSpring(p1_windowButtonGroup)
    gbutton("Ok", container=p1_windowButtonGroup,
        handler=function(h,...) {
          if( length(selTab[])==0 ) {
            gmessage("You need to select at least 1 variable!", title="Information", icon="info", parent=p1_window)
          } else {
            var <- selTab[]
            svar <- sTab[]
            if(length(svar)==0) svar <- NULL
            pram_tmp(var, svar)
            
            dispose(p1_window)
          } 
        })
    gbutton("Cancel", container=p1_windowButtonGroup, handler=function(h,...) { dispose(p1_window) })
    gbutton("Help", container=p1_windowButtonGroup, handler=function(h,...) { helpR("pram_strata") })
  }  
  # function for gr_button2
  # opens script window to execute R commands directly
  # globalRecodeGroup function
  scriptWindow <- function(...) {
    # TODO: auto scroll down needs to be implemented
    scriptEnv = new.env()
    assign("cmdhist", c(), envir=scriptEnv)
    sendCommand <- function(gin, gout, ...) {
      insert(gout, paste(">", svalue(gin)), font.attr=c(color="red", family="monospace"))
      err <- try(res <- capture.output(eval(parse(text=svalue(gin)), envir=scriptEnv), append=FALSE),silent=TRUE)
      if(class(err)!="try-error"){
        if( length(res)>0 ){
          #res <- capture.output(print(res))
          insert(gout, res[1], font.attr=c(family="monospace"))
          if( length(res)>1 ) {
            for( i in 2:length(res) ) {
              insert(gout, res[i], font.attr=c(family="monospace"))
            }
          }
        }
      }else{
        insert(gout, err[1]
            , font.attr=c(family="monospace"))
      }
      if( length(strsplit(svalue(gin), "<-")[[1]])>1 || length(strsplit(svalue(gin), "=")[[1]])>1 ) {
        cmdhist <- get("cmdhist", envir=scriptEnv)
        cmdhist <- c(cmdhist, svalue(gin))
        assign("cmdhist", cmdhist, envir=scriptEnv)
      }
      svalue(gin) <- ""
    }
    saveAds <- function(...) {
      ActiveSdcObject(get("sdc", envir=scriptEnv))
      freqCalcIndivRisk()
      cmdhist <- get("cmdhist", envir=scriptEnv)
      if( length(cmdhist) > 0 ) {
        for( i in 1:length(cmdhist) ) {
          Script.add(cmdhist[i])
        }
      }
      # end save
      freqCalcIndivRisk()
      nm_risk_print_function()
      quitScriptWindow()
    }
    removeWs <- function(...) {
      if( exists("scriptEnv", envir=.GlobalEnv) ) {
        try(rm(scriptEnv, envir=.GlobalEnv), silent=TRUE)
      }
    }
    sureQuit <- function(...) {
      gconfirm("You want to close the window without saving?", icon="question", parent=scriptWindow,
          handler=function(h,...) quitScriptWindow() )
    }
    quitScriptWindow <- function(...) {
      removeWs()
      dispose(scriptWindow)
    }
    loadAds <- function(...) {
      assign("sdc", ActiveSdcObject(), envir=scriptEnv)
      #-- End - summary.freqCalc
    }
    scriptWindow = gwindow("Script window", parent=window)
    scriptWidget = ggroup(horizontal=FALSE)
    scriptInfoGroup = ggroup(container=scriptWidget)
    addSpring(scriptInfoGroup)
    glabel("Active Sdc Object available for modifications as variable: sdc",
        container=scriptInfoGroup)
    gbutton("Reload active data set to sdc", container=scriptInfoGroup,
        handler=function(h,...) loadAds() )
    addSpring(scriptInfoGroup)
    loadAds()
    xout = gtext(text="", width=700, height=400)
    add(scriptWidget, xout)
    scriptSubmit = ggroup(container=scriptWidget)
    glabel(" >", container=scriptSubmit)
    xcom = gedit("", container=scriptSubmit, expand=TRUE)#, handler=function(h, ...) sendCommand(xcom, xout))
    gbutton("submit", container=scriptSubmit, handler=function(h, ...) sendCommand(xcom, xout))
    gseparator(container=scriptWidget)
    saveCancelGroup = ggroup(container=scriptWidget)
    addSpring(saveCancelGroup)
    gbutton("Overwrite ads", container=saveCancelGroup, handler=function(h,...) saveAds() )
    gbutton("Cancel", container=saveCancelGroup, handler=function(h,...) sureQuit() )
    
    add(scriptWindow, scriptWidget)
    focus(xcom)
  }
  
  # TODO: nm_risk_print_function
  # nm_risk_print output function
  nm_risk_print_function <- function(...) {
    if(length(ActiveSdcVars("numVars"))>0){
      xprogress = gwindow("please wait", width=180, height=40, parent=window)
      glabel("... script running ...", container=xprogress)
      optionss <- ActiveSdcVars("options")
      #if(!identical(svalue(nm_risk_slider1),optionss$risk_k)|!identical(svalue(nm_risk_slider2),optionss$risk_k2)){
      #  optionss$risk_k <- svalue(nm_risk_slider1)
      #  optionss$risk_k2 <- svalue(nm_risk_slider2)
        obj <- ActiveSdcObject()
      #  obj@options <- optionss
        ActiveSdcObject(dUtility(dRisk(obj)))
      #}
      
      risk <- ActiveSdcVars("risk")
      originalRisk <- ActiveSdcVars("originalRisk")
      utility <- ActiveSdcVars("utility")
    
      svalue(nm_risk_print) <- paste("Disclosure Risk is between: \n [0% ; ", 
			  round(100*risk$numeric,2), "%] (current)\n 
				(orig: ~", 100, "%) \n",sep="")

svalue(nm_util_print) <- paste("- Information Loss:\n    IL1: ", 
		round(utility$il1,2),"\n  - Difference Eigenvalues: ",round(utility$eigen*100,2)," %",
    "\n\n (orig: Information Loss: 0) \n",sep="")
      dispose(xprogress)
    }
  }
  
  generateStrata_tmp <- function(stratavars,name){
    putd("sLen", 1)
    putd("sVars",name)
    
    xtmp <- ActiveDataSet()
    strata <- rep("",nrow(xtmp))
    for(i in 1:length(stratavars)){
      strata <- paste(strata,xtmp[,stratavars[i]],sep="")
      if(length(stratavars)>i)
        strata <- paste(strata,"-",sep="")
    }
    xtmp <- cbind(xtmp,strata)
    colnames(xtmp)[length(colnames(xtmp))] <- name
    putd("activeDataSet", xtmp)
    putd("sIndex", getIndex(name))
  }
  selVar <- function(...) {
    putd("keyLen", 0)
    putd("numLen", 0)
    putd("wLen", 0)
    putd("hLen", 0)
    putd("sLen", 0)
    #print(ActiveDataSet())
    ft <- function(f, t, h, var, pm, ...) {
      # pm: 1 for +, 0 for -
      count = getd(var) 
      if( pm == 1 ) {
        count <- count + length(h);
      } else {
        count <- count - length(h);
      }
      putd(var, count)
      if( length(h)>0 ) {
        if( length(f[])==1 ) {
          if( is.na(f[]) ) {
            f[,] <- data.frame(vars=h, stringsAsFactors=FALSE)
          } else {
            f[,] <- data.frame(vars=c(f[], h), stringsAsFactors=FALSE)
          }
        } else {
          f[,] <- data.frame(vars=c(f[], h), stringsAsFactors=FALSE)
        }
        if( length(h)==length(t[]) ) {
          t[,] <- data.frame(vars=character(0), stringsAsFactors=FALSE)
        } else {
          xtmp <- c()
          for( i in 1:length(t[]) ) {
            for( j in 1:length(h) ) {
              if( t[][i]==h[j] ) {
                xtmp <- c(xtmp, i)
              }
            }
          }
          t[,] <- data.frame(vars=t[-xtmp], stringsAsFactors=FALSE)
        }
        f[,] <- names(ActiveDataSet())[names(ActiveDataSet())%in%f[,]]
        t[,] <- names(ActiveDataSet())[names(ActiveDataSet())%in%t[,]]
      }
    }
    selVar_window = gwindow("Select variables", width=230, parent=window,height=700)
    selVar_windowGroup = ggroup(container=selVar_window, horizontal=FALSE)
    selVar_main = ggroup(container=selVar_windowGroup)
    mtmp = ggroup(container=selVar_main)
    
    allVars <-names(ActiveDataSet())
    
    
    
    
    
    # If it is not the first call to selVar, the previous selection is read
    if(existd("sdcObject")){
      sdcObject <- getd("sdcObject")
      
      keyVars <- ActiveSdcVarsStr("keyVars")
      numVars <- ActiveSdcVarsStr("numVars")
      wVars <- ActiveSdcVarsStr("weightVar")
      sVars <- ActiveSdcVarsStr("strataVar")
      hVars <- ActiveSdcVarsStr("hhId")
      putd("numLen", length(numVars))
      putd("keyLen", length(keyVars))
      putd("hLen", length(hVars))
      putd("wLen", length(wVars))
      putd("sLen", length(sVars))
    }else{
      numVars <- keyVars <- hVars <- wVars <- sVars <- character(0)
    }
    #Not selected variables
    nsVars <- allVars[!allVars%in%c(numVars,keyVars,hVars,wVars,sVars)]
    #numVars <- c()
    #xtmp <- ActiveDataSet()
    #for( i in 1:dim(xtmp)[2] ) {
    #	numVars <- c(numVars, names(xtmp)[i])
    #}
    varTab = gtable(data.frame(vars=nsVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(varTab) <- c(120,400)
    add(mtmp, varTab)
    
    rtmp = ggroup(container=mtmp, horizontal=FALSE)
    
    tmp = gframe("categorical", container=rtmp)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(catTab, varTab, svalue(varTab), "keyLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, catTab, svalue(catTab), "keyLen", 0) })
    addSpring(btmp)
    catTab = gtable(data.frame(vars=keyVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(catTab) <- c(120,150)
    add(tmp, catTab)
    
    tmp = gframe("numerical", container=rtmp)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(numTab, varTab, svalue(varTab), "numLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, numTab, svalue(numTab), "numLen", 0) })
    addSpring(btmp)
    numTab = gtable(data.frame(vars=numVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(numTab) <- c(120,150)
    add(tmp, numTab)
    
    tmp = gframe("weight", container=rtmp)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(wTab, varTab, svalue(varTab), "wLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, wTab, svalue(wTab), "wLen", 0) })
    addSpring(btmp)
    wTab = gtable(data.frame(vars=wVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(wTab) <- c(120,50)
    add(tmp, wTab)
    ##Household Selection
    tmp = gframe("household id", container=rtmp)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(hTab, varTab, svalue(varTab), "hLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, hTab, svalue(hTab), "hLen", 0) })
    addSpring(btmp)
    hTab = gtable(data.frame(vars=hVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(hTab) <- c(120,50)
    add(tmp, hTab)
    
    tmp = gframe("strata", container=rtmp)
    btmp = ggroup(container=tmp, horizontal=FALSE)
    addSpring(btmp)
    gbutton(">>", container=btmp, handler=function(h,...) { ft(sTab, varTab, svalue(varTab), "sLen", 1) })
    gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, sTab, svalue(sTab), "sLen", 0) })
    addSpring(btmp)
    sTab = gtable(data.frame(vars=sVars, stringsAsFactors=FALSE), multiple=TRUE)
    size(sTab) <- c(120,100)
    add(tmp, sTab)
    
    
    
    gseparator(container=selVar_windowGroup)
    selVar_windowButtonGroup = ggroup(container=selVar_windowGroup)
    addSpring(selVar_windowButtonGroup)
    b1 <- gbutton("Generate Strata Variable", container=selVar_windowButtonGroup, handler=function(h,...) {
          #confirmSelection_tmp(catTab[], numTab[], wTab[],hTab[],sTab[])
          confirmSelection_tmp(catTab[], numTab[], wTab[],hTab[],sTab[])    
          dispose(selVar_window)
          stVar_window = gwindow("Generate a strata variable", width=230, parent=window)
          stVar_windowGroup = ggroup(container=stVar_window, horizontal=FALSE)
          stVar_main = ggroup(container=stVar_windowGroup)
          mtmp = ggroup(container=stVar_main)
          allVars <-colnames(ActiveDataSet())
          rmIndex <- c()
          nro <- nrow(ActiveDataSet())
          for(i in 1:length(allVars)){
            if(nrow(unique(ActiveDataSet()[,allVars[i],drop=FALSE]))>nro*.2)
              rmIndex <- c(rmIndex,i)
          }
          allVars <- allVars[-rmIndex]
          varTab = gtable(data.frame(vars=allVars, stringsAsFactors=FALSE), multiple=TRUE)
          size(varTab) <- c(120,400)
          add(mtmp, varTab)
          rtmp = ggroup(container=mtmp, horizontal=FALSE)
          tmp = gframe("strata", container=rtmp)
          btmp = ggroup(container=tmp, horizontal=FALSE)
          addSpring(btmp)
          gbutton(">>", container=btmp, handler=function(h,...) { ft(sTab, varTab, svalue(varTab), "sLen", 1) })
          gbutton("<<", container=btmp, handler=function(h,...) { ft(varTab, sTab, svalue(sTab), "sLen", 0) })
          addSpring(btmp)
          sTab = gtable(data.frame(vars=sVars, stringsAsFactors=FALSE), multiple=TRUE)
          size(sTab) <- c(120,400)
          add(tmp, sTab)
          gseparator(container=stVar_windowGroup)
          stVar_windowButtonGroup = ggroup(container=stVar_windowGroup)
          addSpring(stVar_windowButtonGroup)
          gbutton("Ok", container=stVar_windowButtonGroup,
              handler=function(h,...) {
                name <- "sdcMicroStrataVariable"
                sVars <- sTab[]
                if(length(sVars)==0)
                  gmessage("You have to select at least  one categoric variable to generate a strata variable.",
                      title="Information", icon="warning", parent=window)
                else{
                  name <- paste(paste(sVars,collapse="_"),"_stratavar",sep="")
                  t1 <- paste("c(",paste("\"",sVars,"\"",sep="",collapse=","),")",sep="")
                  Script.add(paste("generateStrata_tmp(", t1, ", \"",name,"\")",sep=""))
                  generateStrata_tmp(sVars,name)
                  dispose(stVar_window)
                  selVar()
                }
                
              })
          gbutton("Cancel", container=stVar_windowButtonGroup, handler=function(h,...) { dispose(stVar_window) })
          
        })
    tooltip(b1) <- tt_genstrat 
    gbutton("Ok", container=selVar_windowButtonGroup,
        handler=function(h,...) {
          # check if firstrun - if not reset script and dataset to original one
          #cat(paste(getd("keyLen"), getd("numLen"), getd("wLen"), getd("hLen"), "\n"))
          fr_do <- TRUE
          if( !getd("firstRun") ) {
            fr_do <- gconfirm("If you reselect vars, script and dataset will reset.\nAre you sure?", title="Attention",
                icon="warning", parent=window)
            if( fr_do ) {
              frS <- Script()$cmd[2]
              Script.new()
              if(existd("cmdimp")){
                Script.add(getd("cmdimp"))
                rmd("cmdimp")
              }else if(substring(frS,1,16)=="activedataset <-"){
                Script.add(frS)
              }
              if( existd("oldsdcObject") ) {
                putd("sdcObject", getd("oldsdcObject"))
              }
            }
          } else {
            putd("firstRun", FALSE)
          }
          # check if enough is selected
          if( fr_do ) {
            # min selection must be 1 in each category
            if( ((getd("keyLen")>=1  || getd("numLen")>=1))&&getd("wLen")%in%c(0,1)&&getd("hLen")%in%c(0,1)) {
              keyVars <- catTab[]
              confirmSelection_tmp(catTab[], numTab[], wTab[],hTab[],sTab[])
              dispose(selVar_window)
              if(getd("keyLen")>=1){
                keyV <- keyVars
                keynofac <- keyV[!as.vector(sapply(keyV,function(x)is.factor(ActiveDataSet()[,x])))]
                if(length(keynofac)>0){
                  keynofac <- paste(keynofac,collapse=",")
                  gmessage(paste("The variables ",keynofac," are selected as categoric but are not of type factor.
                              In the next window you can change this, you can reopen this window by clicking \"Recode\"",sep=""),
                      title="Information", parent=window)
                  vc()
                }
              }
            } else {
              gmessage("You have to select at least  one numeric or categoric variable and optional one weight variable, one household ID variable and several strata variables.",
                  title="Information", icon="warning", parent=window)
            }
          }
        })
    gbutton("Cancel", container=selVar_windowButtonGroup, handler=function(h,...) { dispose(selVar_window) })
  }
  
  
  # function for gb1 (confirm selection)
  # needed sub functions
  # TODO: done - save selection for script/history
writeVars <- function(t1,t2,t3,t4,t5){
  svalue(dslab) <- paste(getd("dataSetName")," [n=",nrow(ActiveDataSet()),"]",sep="")
  enabled(gb1) <- TRUE
  enabled(gb2) <- TRUE
  if(length(t1)>0){
    stmp <- ""
    for( i in 1:length(t1) ) {
      stmp <- paste(stmp,t1[i]," [#:",length(unique(ActiveDataSet()[,t1[i]])),"]\n",sep="")
    } 
    svalue(tab1) <- stmp
    enabled(ir_button) <- TRUE
    enabled(vk_button) <- TRUE
    enabled(vh_button) <- TRUE
    enabled(ls_button1) <- TRUE
    enabled(ld_button1) <- TRUE
    enabled(pram_button1) <- TRUE
    #enabled(ls_button2) <- TRUE
    enabled(vc_button1) <- TRUE
    #enabled(gr_button2) <- TRUE
  }else{
    svalue(tab1) <- "not selected\n"
    enabled(ir_button) <- FALSE
    enabled(vk_button) <- FALSE
    enabled(vh_button) <- FALSE
    enabled(ls_button1) <- FALSE
    enabled(ld_button1) <- FALSE
    enabled(pram_button1) <- FALSE
    #enabled(ls_button2) <- TRUE
    enabled(vc_button1) <- FALSE
    #enabled(gr_button2) <- FALSE
  }
  if(length(t2)>0){
    stmp <- ""
    for( i in 1:length(t2) ) {
      mi <- round(min(ActiveDataSet()[,t2[i]],na.rm=TRUE),1)
      ma <- round(max(ActiveDataSet()[,t2[i]],na.rm=TRUE),1)
      me <- round(median(ActiveDataSet()[,t2[i]],na.rm=TRUE),1)
      stmp <- paste(stmp, t2[i],"[Min:",mi,", Med:",me,", Max:",ma,"]\n")
    }
    svalue(tab2) <- stmp
    enabled(nm_button1) <- TRUE
    enabled(shuffle_button1) <- TRUE
    enabled(nm_button2) <- TRUE
    #enabled(nm_button3) <- TRUE
    #enabled(nm_risk_slider1) <- TRUE
    #enabled(nm_risk_slider2) <- TRUE
  }else{
    svalue(tab2) <- "not selected\n"
    enabled(nm_button1) <- FALSE
    enabled(shuffle_button1) <- FALSE
    enabled(nm_button2) <- FALSE
    #enabled(nm_button3) <- FALSE
    #enabled(nm_risk_slider1) <- FALSE
    #enabled(nm_risk_slider2) <- FALSE
  }
  if(length(t3)>0){
    stmp <- ""
    for( i in 1:length(t3) ) {
      mi <- round(min(ActiveDataSet()[,t3[i]],na.rm=TRUE),1)
      ma <- round(max(ActiveDataSet()[,t3[i]],na.rm=TRUE),1)
      me <- round(median(ActiveDataSet()[,t3[i]],na.rm=TRUE),1)
      stmp <- paste(stmp, t3[i]," [Min:",mi,", Med:",me,", Max:",ma,"]\n",sep="")
    }
    svalue(tab3) <- stmp
  }else
    svalue(tab3) <- "not selected\n"
  if(length(t4)>0){
    stmp <- ""
    for( i in 1:length(t4) ) {
      me <- round(mean(by(ActiveDataSet()[,t4[i]],ActiveDataSet()[,t4[i]],length),na.rm=TRUE),1)
      stmp <- paste(stmp, t4[i]," [Mean size:",me,"]",sep="")
    }
    svalue(tab4) <- stmp
  }else 
    svalue(tab4) <- "not selected\n"
  if(length(t5)>0){
    stmp <- ""
    for( i in 1:length(t5) ) {
      stmp <- paste(stmp,t5[i]," [#:",length(unique(ActiveDataSet()[,t5[i]])),"]\n",sep="")
    }
    svalue(tab5) <- stmp
  }else 
    svalue(tab5) <- "not selected\n"
  
  # enable plot indivRisk button
  freqCalcIndivRisk()
  nm_risk_print_function()
}
  confirmSelection_tmp <- function(t1=character(0), t2=character(0), t3=character(0),t4=character(0),t5=character(0)) {
    selvar <- vector()
    if(length(t1)>0)
      selvar[length(selvar)+1] <- paste("keyVars=",parseVarStr(t1),sep="")
    if(length(t2)>0)
      selvar[length(selvar)+1] <- paste("numVars=",parseVarStr(t2),sep="")
    if(length(t3)>0)
      selvar[length(selvar)+1] <- paste("weightVar=",parseVarStr(t3),sep="")
    if(length(t4)>0)
      selvar[length(selvar)+1] <- paste("hhId=",parseVarStr(t4),sep="")
    if(length(t5)>0)
      selvar[length(selvar)+1] <- paste("strataVar=",parseVarStr(t5),sep="")
    selvar <- paste(selvar,collapse=",")
    if(existd("cmdimp")){
      Script.add(getd("cmdimp"))
      rmd("cmdimp")
    }
    Script.add(paste("sdcObject <- createSdcObj(activedataset,", selvar, ")", sep=""))
    xprogress = gwindow("please wait", width=180, height=40, parent=window)
    glabel("... script running ...", container=xprogress)
    if(existd("importFilename"))
      filename <- getd("importFilename")
    else
      filename <- getd("dataSetName")
    sdcObject <- createSdcObj(ActiveDataSet(),keyVars=t1,numVars=t2,weightVar=t3,hhId=t4,strataVar=t5,options=list(risk_k=0.01,risk_k2=0.05,filename=filename))
    dispose(xprogress)
    ActiveSdcObject(sdcObject)
    writeVars(t1,t2,t3,t4,t5)    
    
  }
  # variableSelectionGroup function
  #       if re-clicked, prompt and ask if you want to reset all work and script done
  # 			this is to be used to set dataset to start format as well as reset script,
  #				because it is not needed to reselect the vars during the work process
  confirmSelection <- function(...) {
    # open selection window
    #if(existd("sdcObject"))
    #  rmd("sdcObject")
    selVar()
  }
  
  ## Menubar Functions
  vign <- function(...) print(vignette("gWidgets"))
  vign2 <- function(...) print(vignette("sdcMicroPaper"))
  paind <- function(...)print(help(package="sdcMicro"))
  
  
  # Data - Load Dataset
  loadDataSet <- function(...) {
    xname <- gfile("Select file to load", parent=window, type="open" ,filter=list("R-Data"=list(patterns=c("*.rda", "*.RData","*.RDA","*.rdata","*.RDATA")), "All files" = list(patterns = c("*"))))
    putd("importFilename",xname)
    if( xname != '' ) {
      load(xname, envir=.GlobalEnv)
    }
    setDataSet()
  }
  # Data - Choose Dataset
  setDataSet <- function(...) {
    vardt <- c("testdata","free1",ls(envir = .GlobalEnv, all.names=TRUE))
    vards <- names(which(sapply(vardt, function(.x) is.data.frame(get(.x)))))
    vards <- c(vards,names(which(sapply(vardt, function(.x) is.matrix(get(.x))))))
    if( length(vards)==0 ) {
      gmessage("No datasets loaded.", title="Information", icon="warning",
          parent=window)
    } else {
      gbasicdialog(title="Choose Dataset",
          x<-gdroplist(vards), parent=window,
          handler=function(x, ...) {
            Script.add(paste("activedataset <- ",svalue(x$obj),sep=""))
            
            ActiveDataSet(svalue(x$obj))
          })
      if( existd("activeDataSet") ) {
        if( dim(ActiveDataSet())[1] > 4000 ) {
          gmessage("Operations in this dataset may require some time, so please be patient.", title="Information",
              icon="info", parent=window)
        }
        svalue(dslab) <- paste(getd("dataSetName")," [n=",nrow(ActiveDataSet()),"]",sep="")
        enabled(gb1) <- TRUE
        enabled(gb2) <- TRUE
      }
      if(existd("sdcObject"))
        rmd("sdcObject")
      selVar()
    }
  }
  # Data - Save Dataset To - File
  saveToFile <- function(...) {
    saveVar <- function(fileName, ...) {
      xtmp <- sdcGUIoutput()
      save(xtmp, file=paste(fileName,".RData", sep=""))
    }
    if( existd("sdcObject") ) {
      xname <- gfile("Choose a file to save the Dataset", type="save", parent=window)
      if( xname != "" ) {
        saveVar(xname)
      }
    } else {
      gmessage("No active Dataset found.", title="Information", icon="warning",
          parent=window)
    }
  }
  # Data - Save Dataset To - Variable
#  saveToVariable <- function(...) {
#    checkAndSave <- function(parent, varName, ...) {
#      saveVar <- function(varName, ...) {
#        #assign(varName, ActiveDataSet(), envir=sdcGUIenv)
#      }
#      if( exists(varName, envir=.GlobalEnv) ) {
#        gconfirm("Variable already exists, dsetDataSet()o you want to replace it?",
#            title="Information", parent=parent,
#            handler=function(h, ...) { saveVar(varName) } )
#      } else {
#        saveVar(varName)
#      }
#    }
#    if( existd("activeDataSet") ) {
#      xname = ginput("Please enter a Variable name",
#          title="Choose Variable name", icon="question", parent=window,
#          handler=function(h, ...) checkAndSave(h$obj, h$input) )
#    } else {
#      gmessage("No active Dataset found.", title="Information", icon="warning",
#          parent=window)
#    }
#  }
#Saving to an R-Object from the GUI is not possibly due to CRAN policy!?
#TODO: Maybe there is a possible work around.
  
  saveToVariable <- function(...) {
    gmessage("Please use the function 'sdcGUIoutput' to assign the current dataset from the sdcGUI to a R-Object, e.g. 'datX <- sdcGUIoutput()'", title="Information", icon="warning",
        parent=window)
  }
  # Typ Dialog
  typDialog <- function(...){
    testimport <- getd("dframe")
    colclasses <- lapply(testimport, class)
    colname <- colnames(testimport)
    importDialog <- getd("importDialog")
    tDialog <- gwindow("Change Variable Types", width=50, height=50,parent=importDialog)
    tGroup <- ggroup(horizontal=FALSE, container=tDialog)
    tFrame <- gframe("select variable type:")
    gg <- glayout(use.scrollwindow = TRUE)
    comboboxes <- list()
    #maximal colums length
    rn <- 10
    for(i in 1:length(colclasses)){
      s <- 0
# 			  if(colclasses[i]=='numeric')s=1
# 			  else if(colclasses[i]=='factor')s=2
# 			  else if(colclasses[i]=='character')s=3
# 			  else if(colclasses[i]=='integer')s=4
      if(colclasses[i]=='numeric')s=1
      else if(colclasses[i]=='factor')s=2
      else if(colclasses[i]=='character')s=2
      else if(colclasses[i]=='integer')s=1
      #print(paste(colname[i],(i-1%%rn)+1))
      gg[((i-1)%%rn)+1,1+2*ceiling(i/rn), anchor=c(0,0)]<-glabel(colname[i])
# 			  gc <- gcombobox(items=c('numeric','factor', 'character', 'integer'), selected=s)
      gc <- gcombobox(items=c('numeric','factor'), selected=s)
      gg[((i-1)%%rn)+1,2+2*ceiling(i/rn)]<-gc
      comboboxes <- c(comboboxes, gc)
    }
    typeaccept <- gbutton("Accept ", handler=function(...){
          testimport <- getd("dframe")
          for(i in 1:length(colclasses)){
            colclasses[i]<-svalue(comboboxes[[i]])
          }
          #print(colclasses)
          #putd("dframe", testimport)
          putd("colclasses",colclasses)
          putd("changedTypes", TRUE)
          dispose(tDialog)
        })
    typediscard <- gbutton("Discard ", handler=function(...){dispose(tDialog)})
# 		  gg[rn+1,1+2*ceiling(length(colclasses)/rn)] <- typeaccept
# 		  gg[rn+1,2+2*ceiling(length(colclasses)/rn)] <- typediscard
    add(tFrame, gg)
    add(tGroup, tFrame)
    tg <- ggroup(horizontal=TRUE)
    addSpring(tg)
    add(tg, typediscard)
    add(tg, typeaccept)
    add(tGroup, tg)
  }
  # Data - Import - Import CSV
  importCSV <- function(...){
    importDialog <- gwindow("Import CSV", parent=window, width=400, height=800)
    putd("importDialog",importDialog)
    putd("dframe", NULL)
    putd("changedTypes", FALSE)
    importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
    layout <- glayout()
    csvfilename <- gedit()
    enabled(csvfilename) <- FALSE
    buttonHandler <- function(...){
      gfile(text = "Open CSV File", type = "open", 
          filter=list("CSV files"=list(patterns=c("*.csv", "*.CSV")), "All files" = list(patterns = c("*"))),
          handler=function(h,...){
            svalue(csvfilename) <- h$file
            tryCatch({
                  fl <- readLines(svalue(csvfilename), n=2)
                  comma <- sapply(strsplit(as.character(fl[1]), ","), length)
                  semicolon <- sapply(strsplit(as.character(fl[1]), ";"), length)
                  dot <- sum(sapply(strsplit(as.character(fl[2]), "."), length))
                  comma2 <- sum(sapply(strsplit(as.character(fl[2]), ","), length))
                  if(comma > semicolon){
                    svalue(csvseperator) <- ","
                    svalue(csvdecimal) <- "."
                  }else{
                    svalue(csvseperator) <- ";"
                    if(comma2 > dot){
                      svalue(csvdecimal) <- ","
                    }
                    else{
                      svalue(csvdecimal) <- "."
                    }
                  }
                },
                error=function(e){
                  gmessage(paste("There was a problem while preparing your data: '",e,"'"), "Problem",
                      icon="error")       
                })
            previewCSV()
          })
    }
    
    
    
    #creates the actual preview inside the table, also the handler for all gui elements
    #beside the OK-button
    previewCSV <- function(...){
      #testdata <- as.data.frame(matrix(rnorm(100), 10, 10))
      f <- gframe("Preview:")
      g <- ggroup(use.scrollwindow = TRUE)
      testimport <- NULL
      error <- FALSE
      if(svalue(csvfilename)==''){
        testimport <- data.frame(column="preview loading ...")
      }
      else{
        svalue(statusbar) <- "compiling preview!"
        tryCatch({testimport <- read.table(svalue(csvfilename), nrows=10,
                  fill=svalue(csvfill),
                  header=svalue(csvheader),
                  strip.white=svalue(csvstrip.white),
                  stringsAsFactors=svalue(csvstringsAsFactors),
                  blank.lines.skip=svalue(csvblank.lines.skip), 
                  sep=svalue(csvseperator),
                  dec=svalue(csvdecimal),
                  quote=svalue(csvquotes),
                  skip=svalue(csvskip),
                  na.strings=strsplit(svalue(csvnastrings),",")[[1]])
              putd("colclasses",NA)
              putd("changedTypes",FALSE)}, 
            error=function(e){svalue(statusbar) <- "read.table was not successful, please check your settings";
              error<-TRUE})
      }
      if(is.null(testimport)==FALSE){
        svalue(statusbar) <- "preview complete!"
      }
      else{
        testimport <- data.frame(column="preview loading ...")
      }
      add(g, gtable(testimport), expand=TRUE)
      add(f, g, expand=TRUE)
      layout[6:10, 1:7, expand=TRUE] <- f
      putd("dframe",testimport)
      
    }
    
    #setup csv import gui
    statusbar <- gstatusbar("")
    csvfilebutton <- gbutton("...", handler=buttonHandler)
    csvheader <- gcheckbox("header", checked=TRUE, handler=previewCSV)
    csvfill <- gcheckbox("fill", checked=TRUE, handler=previewCSV)
    csvstrip.white <- gcheckbox("strip white", , handler=previewCSV)
    csvblank.lines.skip <- gcheckbox("blank line skip", handler=previewCSV)
    csvstringsAsFactors <- gcheckbox("strings As Factors", handler=previewCSV)
    csvseperator <- gedit(",", handler=previewCSV)
    addHandlerKeystroke(csvseperator, previewCSV)
    csvdecimal <- gedit(".", handler=previewCSV)
    addHandlerKeystroke(csvdecimal, previewCSV)
    csvquotes <- gedit("\"", handler=previewCSV)
    addHandlerKeystroke(csvquotes, previewCSV)
    csvskip <- gedit("0")
    addHandlerKeystroke(csvskip, previewCSV)
    csvnastrings <- gedit("")
    addHandlerKeystroke(csvnastrings, previewCSV)
    csvaccept <- gbutton("Accept", handler=function(...){
          ###real CSV import after pressing the accept button
          tryCatch({testimport <- getd("dframe")
                if(getd("changedTypes")==TRUE){
                  colclasses <- getd("colclasses")
                  colclassesSTR <- parseVarStr(colclasses)
                }  
                else{
                  colclasses <- NA
                  colclassesSTR <- "NA"
                }
                wd <- WaitingDialog(Parent=importDialog)
                focus(wd) <- TRUE
                putd("importFilename",svalue(csvfilename))
                filename=gsub("\\\\","/",svalue(csvfilename))
                df <- read.table(svalue(csvfilename),
                    fill=svalue(csvfill),
                    header=svalue(csvheader),
                    strip.white=svalue(csvstrip.white),
                    stringsAsFactors=svalue(csvstringsAsFactors),
                    blank.lines.skip=svalue(csvblank.lines.skip), 
                    sep=svalue(csvseperator),
                    dec=svalue(csvdecimal),
                    quote=svalue(csvquotes),
                    skip=svalue(csvskip),
                    colClasses=colclasses,
                    na.strings=strsplit(svalue(csvnastrings),",")[[1]])
                dname <- format(Sys.time(), "importedCSV_%H_%M")
                cmdimp <- paste("activedataset <- read.table(\"",filename,"\"",
                    ",fill=",svalue(csvfill), 
                    ",header=",svalue(csvheader),
                    ",strip.white=",svalue(csvstrip.white),
                    ",stringsAsFactors=",svalue(csvstringsAsFactors),
                    ",blank.lines.skip=",svalue(csvblank.lines.skip),
                    ",sep=",parseVarStr(svalue(csvseperator)),
                    ",dec=",parseVarStr(svalue(csvdecimal)),
                    ",quote=\"\\",svalue(csvquotes),"\"",
                    ",skip=",parseVarStr(svalue(csvskip)),
                    ",colClasses=",colclassesSTR,
                    ",na.strings=",parseVarStr(svalue(strsplit(svalue(csvnastrings),",")[[1]])),
                    ")", sep="")
                putd("cmdimp",cmdimp)
                putd("activeDataSet", df)
                putd("dataSetName",dname)
                putd("oldDataSet", ActiveDataSet())
                svalue(dslab) <- paste(getd("dataSetName")," (n=",nrow(ActiveDataSet()),")",sep="")
                enabled(gb1) <- TRUE
                enabled(gb2) <- TRUE
                putd("numLen", 0)
                putd("numVars", character(0))
                putd("keyLen", 0)
                putd("keyVars", character(0))
                putd("hLen", 0)
                putd("hVars", character(0))
                putd("wLen", 0)
                putd("wVars", character(0))
                putd("sLen", 0)
                putd("sVars", character(0))
                putd("importFileName", svalue(csvfilename))
                #save import parameters for later export
                csvimportparams <- list(fill=svalue(csvfill),
                    header=svalue(csvheader),
                    strip.white=svalue(csvstrip.white),
                    stringsAsFactors=svalue(csvstringsAsFactors),
                    blank.lines.skip=svalue(csvblank.lines.skip), 
                    sep=svalue(csvseperator),
                    dec=svalue(csvdecimal),
                    quote=svalue(csvquotes),
                    skip=svalue(csvskip),
                    colClasses=colclasses,
                    na.strings=strsplit(svalue(csvnastrings),",")[[1]])
                putd("csvimportparameters", csvimportparams)
                dispose(wd)
                dispose(importDialog)
                if(existd("sdcObject"))
                  rmd("sdcObject")
                selVar()
              },
              error=function(e){gmessage(paste("There was a problem while importing your data: '",e,"'"), "Problem",
                    icon="error")})
          
        })
    csvdiscard <- gbutton("Discard ", handler=function(...){dispose(importDialog)})
    csvadjustTypes <- gbutton("Adjust Types", handler=typDialog)
    
    ftop <- gframe("Choose CSV-File:")
    gtop <- ggroup(horizontal=TRUE, container=ftop)
    add(ftop, csvfilename, expand=TRUE)
    add(ftop, csvfilebutton)
    layout[1,1:7] <- ftop
# 	  layout[1,1, anchor=c(0,0)] <- glabel("Choose CSV-File:")
# 	  layout[1,2:6, fill=TRUE] <- csvfilename
# 	  layout[1,7] <- csvfilebutton
    fparams <- gframe("CSV-Parameters:")
    glayout <- glayout(container=fparams)
    glayout[2,1] <- csvheader
    glayout[3,1] <- csvfill
    glayout[4,1] <- csvstrip.white
    glayout[5,1] <- csvstringsAsFactors
    glayout[2,2] <- csvblank.lines.skip
    glayout[2,3, anchor=c(0,0)] <- glabel("seperator:")
    glayout[2,4] <- csvseperator
    glayout[3,3, anchor=c(0,0)] <- glabel("decimal:")
    glayout[3,4] <- csvdecimal
    glayout[4,3, anchor=c(0,0)] <- glabel("quotes:")
    glayout[4,4] <- csvquotes
    glayout[5,3, anchor=c(0,0)] <- glabel("skip:")
    glayout[5,4] <- csvskip
    glayout[2,5, anchor=c(0,0)] <- glabel("NA-strings:")
    glayout[2,6, expand=FALSE] <- csvnastrings
    layout[2:5, 1:7] <- fparams
    previewCSV()
    layout[11,5, expand=FALSE] <- csvadjustTypes
    layout[11,6, expand=FALSE] <- csvaccept
    layout[11,7, expand=FALSE] <- csvdiscard
    add(importDialogFrame, layout, expand=TRUE)
    add(importDialogFrame, statusbar)
    buttonHandler()
  }
  
  
  # Data - Export - Export CSV
  exportCSV <- function(...){
    if(existd("sdcObject")  == FALSE){
      gmessage("There is no dataset loaded for export!", "No Dataset!",icon="warning")
    }
    else{
      importDialog <- gwindow("Export CSV", parent=window, width=200, height=200)
      putd("importDialog",importDialog)
      putd("dframe", NULL)
      importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
      layout <- glayout()
      csvfilename <- gedit()
      enabled(csvfilename) <- FALSE
      
      buttonHandler <- function(...){
        gfile(text = "Save CSV File", type = "save", filter=list("CSV-Files"=list("*.csv")),handler=function(h,...){
              if(grepl("^.*\\.(csv|CSV)$", h$file)){
                svalue(csvfilename) <- h$file
              }
              else{
                svalue(csvfilename) <- paste(h$file, ".csv", sep="")
              }
            })
      }
      
      #setup csv import gui
      if(existd("csvimportparameters")){
        ip <- getd("csvimportparameters")
        ip$na.strings<-"NA"
      }
      else{
        ip <- list(fill=TRUE,
            header=TRUE,
            strip.white=TRUE,
            stringsAsFactors=TRUE,
            blank.lines.skip=TRUE, 
            sep=",",
            dec=".",
            quote="'",
            skip=0,
            colClasses=NULL,
            na.strings="NA")
      }
      
      statusbar <- gstatusbar("")
      csvfilebutton <- gbutton("...", handler=buttonHandler)
      csvheader <- gcheckbox("header", checked=ip$header)
      csvseperator <- gedit(ip$sep)
      csvdecimal <- gedit(ip$dec)
      csvnastrings <- gedit(ip$na.strings)
      csvaccept <- gbutton("Accept", handler=function(...){
            tryCatch({write.table(sdcGUIoutput(), file=svalue(csvfilename),
                      sep=svalue(csvseperator),
                      na=svalue(csvnastrings),
                      dec=svalue(csvdecimal),
                      row.names=svalue(csvheader))
                  putd("exportFileName", svalue(csvfilename))
                  if(svalue(radio.html, index=TRUE)==2)
                    exportReport()
                },
                error=function(e){gmessage(paste("There was a problem while exporting your data: '",e,"'"), "Problem",
                      icon="error")})
            dispose(importDialog)
          })
      csvdiscard <- gbutton("Discard ", handler=function(...){dispose(importDialog)})
      
      #record export
      frame.html <- gframe("Generate report?")
      radio.html <- gradio(c("no", "yes"), 
          horizontal=TRUE, container=frame.html)
      
      
      ftop <- gframe("Choose CSV-File:")
      gtop <- ggroup(horizontal=TRUE, container=ftop)
      add(ftop, csvfilename, expand=TRUE)
      add(ftop, csvfilebutton)
      layout[1,1:7] <- ftop
      fparams <- gframe("CSV-Parameters:")
      glayout <- glayout(container=fparams)
      glayout[2,1] <- csvheader
      glayout[2,3, anchor=c(0,0)] <- glabel("seperator:")
      glayout[2,4] <- csvseperator
      glayout[3,3, anchor=c(0,0)] <- glabel("decimal:")
      glayout[3,4] <- csvdecimal
      glayout[2,5, anchor=c(0,0)] <- glabel("NA-strings:")
      glayout[2,6, expand=FALSE] <- csvnastrings
      layout[2:5, 1:7] <- fparams
      layout[8, 1:7] <- frame.html
      layout[9,6, expand=FALSE] <- csvaccept
      layout[9,7, expand=FALSE] <- csvdiscard
      add(importDialogFrame, layout, expand=TRUE)
      #add(importDialogFrame, statusbar)
      
    }
    buttonHandler()
  }
  
  # Data - Import - Import SPSS
  importSPSS <- function(...){
    importDialog <- gwindow("Import SPSS", parent=window, width=100, height=100)
    putd("importDialog",importDialog)
    putd("dframe", NULL)
    importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
    layout <- glayout()
    filename <- gedit()
    enabled(filename) <- FALSE
    
    buttonHandler <- function(...){
      gfile(text = "Open SPSS File", type = "open", , 
          filter=list("SPSS files"=list(patterns=c("*.sav", "*.SAV")),"All files" = list(patterns = c("*"))),
          handler=function(h,...){
            svalue(filename) <- h$file
          })
    }
    
    #setup SPSS import gui
    statusbar <- gstatusbar("")
    filebutton <- gbutton("...", handler=buttonHandler)
    check.use.value.labels <- gcheckbox("convert value labels to factors")
    check.lowernames <- gcheckbox("convert variable names to lower case")
    check.force.single <- gcheckbox("force storage mode double to single", checked=TRUE)
    check.charfactor <- gcheckbox("convert character variables to factors")
    csvaccept <- gbutton("Accept", handler=function(...){
          #try to import spss file, if not message error
          tryCatch({
                wd <- WaitingDialog(Parent=importDialog)
                focus(wd) <- TRUE
                df <- spss.get(svalue(filename),
                    use.value.labels = svalue(check.use.value.labels),
                    lowernames = svalue(check.lowernames),
                    force.single = svalue(check.force.single),
                    charfactor= svalue(check.charfactor),
                    to.data.frame = TRUE)
                putd("importFilename",svalue(filename))
                filename=gsub("\\\\","/",svalue(filename))
                cmdimp <- paste("activedataset <- spss.get(\"",filename,"\"",
                    ",use.value.labels=",svalue(check.use.value.labels), 
                    ",lowernames=",svalue(check.lowernames),
                    ",force.single=",svalue(check.force.single),
                    ",charfactor=",svalue(check.charfactor),
                    ",to.data.frame = TRUE)", sep="")
                putd("cmdimp",cmdimp)
                dname <- format(Sys.time(), "importedSPSS_%H_%M")
                putd("activeDataSet", df)
                putd("dataSetName",dname)
                putd("oldDataSet", ActiveDataSet())
                svalue(dslab) <- paste(getd("dataSetName")," (n=",nrow(ActiveDataSet()),")",sep="")
                enabled(gb1) <- TRUE
                enabled(gb2) <- TRUE
                putd("numLen", 0)
                putd("numVars", character(0))
                putd("keyLen", 0)
                putd("keyVars", character(0))
                putd("hLen", 0)
                putd("hVars", character(0))
                putd("wLen", 0)
                putd("wVars", character(0))
                putd("sLen", 0)
                putd("sVars", character(0))
                dispose(wd)
                dispose(importDialog)
                if(existd("sdcObject"))
                  rmd("sdcObject")
                selVar()
                
              },error=function(e){
                gmessage(paste("There was a problem while importing your SPSS file: '",e,"'"),"Import Error!",icon="error")
              })
          
        })
    csvdiscard <- gbutton("Discard ", handler=function(...){dispose(importDialog)})
    csvadjustTypes <- gbutton("Adjust Types", handler=typDialog)
    
    ftop <- gframe("Choose SPSS-File:")
    gtop <- ggroup(horizontal=TRUE, container=ftop)
    add(ftop, filename, expand=TRUE)
    add(ftop, filebutton)
    layout[1,1:7] <- ftop
    fparams <- gframe("SPSS-Parameters:")
    glayout <- glayout(container=fparams)
    glayout[1,1] <- check.use.value.labels
    glayout[1,2] <- check.lowernames
    glayout[2,1] <- check.force.single
    glayout[2,2] <- check.charfactor
    layout[2:3, 1:7] <- fparams
    layout[4,6, expand=FALSE] <- csvaccept
    layout[4,7, expand=FALSE] <- csvdiscard
    add(importDialogFrame, layout, expand=TRUE)
    buttonHandler()
  }
  
  # Data - Export - Export SPSS
  exportSPSS <- function(...){
    if(existd("activeDataSet")  == FALSE){
      gmessage("There is no dataset loaded for export!", "No Dataset!",icon="warning")
    }
    else{
      importDialog <- gwindow("Export SPSS", parent=window, width=100, height=100)
      putd("importDialog",importDialog)
      putd("dframe", NULL)
      importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
      layout <- glayout()
      datafilename <- gedit()
      codefilename <- gedit()
      enabled(datafilename) <- FALSE
      enabled(codefilename) <- FALSE
      
      databuttonHandler <- function(...){
        gfile(text = "Save Data File", type = "save", handler=function(h,...){
              if(grepl("^.*\\.(dat)$", h$file)){
                svalue(datafilename) <- h$file
              }
              else{
                svalue(datafilename) <- paste(h$file, ".dat", sep="")
              }
            })
      }
      
      codebuttonHandler <- function(...){
        gfile(text = "Save SPS File", type = "save", filter=list(".sps"=list("*.sps")),handler=function(h,...){
              if(grepl("^.*\\.(sps|SPS)$", h$file)){
                svalue(codefilename) <- h$file
              }
              else{
                svalue(codefilename) <- paste(h$file, ".sps", sep="")
              }
            })
      }
      
      #setup sas export gui
      datafilebutton <- gbutton("...", handler=databuttonHandler)
      codefilebutton <- gbutton("...", handler=codebuttonHandler)
      csvaccept <- gbutton("Accept", handler=function(...){
            tryCatch({write.foreign(sdcGUIoutput(), datafile=svalue(datafilename),
                      codefile=svalue(codefilename),
                      package = "SPSS")
                  putd("exportFileName", svalue(datafilename))
                  if(svalue(radio.html, index=TRUE)==2)
                    exportReport()
                  },
                error=function(e){gmessage(paste("There was a problem while exporting your data: '",e,"'"), "Problem",
                      icon="error")})
            dispose(importDialog)
          })
      csvdiscard <- gbutton("Discard ", handler=function(...){dispose(importDialog)})
      
      #record export
      frame.html <- gframe("Generate report?")
      radio.html <- gradio(c("no", "yes"), 
          horizontal=TRUE, container=frame.html)
      
      fdata <- gframe("Choose Data-File (Contains exported data as freetext):")
      gdata <- ggroup(horizontal=TRUE, container=fdata)
      add(fdata, datafilename, expand=TRUE)
      add(fdata, datafilebutton)
      layout[1,1:7] <- fdata
      
      fcode <- gframe("Choose Code-File (Contains SPSS Code for import):")
      gcode <- ggroup(horizontal=TRUE, container=fcode)
      add(fcode, codefilename, expand=TRUE)
      add(fcode, codefilebutton)
      layout[2,1:7] <- fcode
      layout[3, 1:7] <- frame.html
      layout[4,6, expand=FALSE] <- csvaccept
      layout[4,7, expand=FALSE] <- csvdiscard
      add(importDialogFrame, layout, expand=TRUE)
      #add(importDialogFrame, statusbar)
      
    }
    
  }
  
  # Data - Import - Import STATA
  loadSTATA <- function(...){
    importDialog <- gwindow("Import STATA", parent=window, width=100, height=100)
    putd("importDialog",importDialog)
    putd("dframe", NULL)
    importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
    layout <- glayout()
    filename <- gedit()
    enabled(filename) <- FALSE
    
    buttonHandler <- function(...){
      gfile(text = "Open STATA File", 
          filter=list("STATA files"=list(patterns=c("*.dta", "*.DTA")),"All files" = list(patterns = c("*"))),
          type = "open", handler=function(h,...){
            svalue(filename) <- h$file
          })
    }
    
    #setup STATA import gui
    statusbar <- gstatusbar("")
    filebutton <- gbutton("...", handler=buttonHandler)
    check.use.value.labels <- gcheckbox("convert value labels to factors", checked=TRUE)
    csvaccept <- gbutton("Accept", handler=function(...){
          #try to import stata file, if not message error
          tryCatch({
                wd <- WaitingDialog(Parent=importDialog)
                focus(wd) <- TRUE
                
                df <- read.dta(svalue(filename),
                    convert.factors = svalue(check.use.value.labels))
                putd("importFilename",svalue(filename))
                filename=gsub("\\\\","/",svalue(filename))
                cmdimp <- paste("activedataset <- read.dta(\"",filename,"\"",
                    ",convert.factors=",svalue(check.use.value.labels),")",sep="")
                putd("cmdimp",cmdimp)
                dname <- format(Sys.time(), "importedSTATA_%H_%M")
                putd("activeDataSet", df)
                putd("dataSetName",dname)
                putd("oldDataSet", ActiveDataSet())
                svalue(dslab) <- paste(getd("dataSetName")," (n=",nrow(ActiveDataSet()),")",sep="")
                enabled(gb1) <- TRUE
                enabled(gb2) <- TRUE
                putd("numLen", 0)
                putd("numVars", character(0))
                putd("keyLen", 0)
                putd("keyVars", character(0))
                putd("hLen", 0)
                putd("hVars", character(0))
                putd("wLen", 0)
                putd("wVars", character(0))
                putd("sLen", 0)
                putd("sVars", character(0))
                dispose(wd)
                dispose(importDialog)
                if(existd("sdcObject"))
                  rmd("sdcObject")
                selVar()
                
              },error=function(e){
                gmessage(paste("There was a problem while importing your STATA file: ",e,"'"),"Import Error!",icon="error")
              })
          
        })
    csvdiscard <- gbutton("Discard ", handler=function(...){dispose(importDialog)})
    csvadjustTypes <- gbutton("Adjust Types", handler=typDialog)
    
    ftop <- gframe("Choose STATA-File:")
    gtop <- ggroup(horizontal=TRUE, container=ftop)
    add(ftop, filename, expand=TRUE)
    add(ftop, filebutton)
    layout[1,1:7] <- ftop
    fparams <- gframe("STATA-Parameters:")
    glayout <- glayout(container=fparams)
    glayout[1,1] <- check.use.value.labels
    layout[2, 1:7] <- fparams
    layout[3,6, expand=FALSE] <- csvaccept
    layout[3,7, expand=FALSE] <- csvdiscard
    add(importDialogFrame, layout, expand=TRUE)
    buttonHandler()
  }
  
  # Data - Export - Export STATA
  exportSTATA <- function(...){
    if(existd("sdcObject")  == FALSE){
      gmessage("There is no dataset loaded for export!", "No Dataset!",icon="warning")
    }
    else{
      importDialog <- gwindow("Export STATA", parent=window, width=100, height=100)
      putd("importDialog",importDialog)
      putd("dframe", NULL)
      importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
      layout <- glayout()
      filename <- gedit()
      enabled(filename) <- FALSE
      edit.version <- gedit("7")
      check.dates <- gcheckbox("convert dates to STATA-dates", checked=TRUE)
      combo.convert.factors <- gcombobox(c("labels","string","numeric","codes"))
      
      buttonHandler <- function(...){
        gfile(text = "Save STATA File", type = "save", filter=list(".dta"=list("*.dta")),handler=function(h,...){
              if(grepl("^.*\\.(dta|DTA)$", h$file)){
                svalue(filename) <- h$file
              }
              else{
                svalue(filename) <- paste(h$file, ".dta", sep="")
              }
            })
      }
      
      #setup stata export gui
      filebutton <- gbutton("...", handler=buttonHandler)
      csvaccept <- gbutton("Accept", handler=function(...){
            tryCatch({write.dta(sdcGUIoutput(), file=svalue(filename),
                      version=as.numeric(svalue(edit.version)),
                      convert.dates=svalue(check.dates),
                      convert.factors=svalue(combo.convert.factors))
                  putd("exportFileName", svalue(filename))
                  if(svalue(radio.html, index=TRUE)==2)
                    exportReport()
                },
                error=function(e){gmessage(paste("There was a problem while exporting your data: '",e,"'"), "Problem",
                      icon="error")})
            dispose(importDialog)
          })
      csvdiscard <- gbutton("Discard ", handler=function(...){dispose(importDialog)})
      
      #record export
      frame.html <- gframe("Generate report?")
      radio.html <- gradio(c("no", "yes"), 
          horizontal=TRUE, container=frame.html)
      
      ftop <- gframe("Choose STATA-File:")
      gtop <- ggroup(horizontal=TRUE, container=ftop)
      add(ftop, filename, expand=TRUE)
      add(ftop, filebutton)
      layout[1,1:7] <- ftop
      fparams <- gframe("STATA-Parameters:")
      glayout <- glayout(container=fparams)
      glayout[1,1] <- check.dates
      glayout[1,2, anchor=c(0,0)] <- glabel("version:")
      glayout[1,6, expand=FALSE] <- edit.version
      glayout[2,2, anchor=c(0,0)] <- glabel("handle factors as:")
      glayout[2,6, expand=FALSE] <- combo.convert.factors
      layout[2:3, 1:7] <- fparams
      layout[4, 1:7] <- frame.html
      layout[5,6, expand=FALSE] <- csvaccept
      layout[5,7, expand=FALSE] <- csvdiscard
      add(importDialogFrame, layout, expand=TRUE)
      #add(importDialogFrame, statusbar)
      
    }
    
  }
  
  # Data - Import - Import SAS
  importSAS <- function(...){
    importDialog <- gwindow("Import SAS", parent=window, width=100, height=100)
    putd("importDialog",importDialog)
    putd("dframe", NULL)
    importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
    layout <- glayout()
    filename <- gedit()
    enabled(filename) <- FALSE
    
    buttonHandler <- function(...){
      gfile(text = "Open SAS Export File", 
          filter=list("SAS XPORT"=list(patterns=c("*.xpt", "*.XPT")),"All files" = list(patterns = c("*"))),
          type = "open", handler=function(h,...){
            svalue(filename) <- h$file
          })
    }
    
    #setup SAS import gui
    statusbar <- gstatusbar("")
    filebutton <- gbutton("...", handler=buttonHandler)
    csvaccept <- gbutton("Accept", handler=function(...){
          #try to import sas file, if not message error
          tryCatch({
                wd <- WaitingDialog(Parent=importDialog)
                focus(wd) <- TRUE
                df <- sasxport.get(svalue(filename))
                putd("importFilename",svalue(filename))
                filename=gsub("\\\\","/",svalue(filename))
                cmdimp <- paste("activedataset <- sasxport.get(\"",filename,"\")",sep="")
                putd("cmdimp",cmdimp)
                
                dname <- format(Sys.time(), "importedSAS_%H_%M")
                putd("activeDataSet", df)
                putd("dataSetName",dname)
                putd("oldDataSet", ActiveDataSet())
                svalue(dslab) <- paste(getd("dataSetName")," (n=",nrow(ActiveDataSet()),")",sep="")
                putd("numLen", 0)
                putd("numVars", character(0))
                putd("keyLen", 0)
                putd("keyVars", character(0))
                putd("hLen", 0)
                putd("hVars", character(0))
                putd("wLen", 0)
                putd("wVars", character(0))
                putd("sLen", 0)
                putd("sVars", character(0))
                dispose(wd)
                dispose(importDialog)
                if(existd("sdcObject"))
                  rmd("sdcObject")
                selVar()
                
              },error=function(e){
                gmessage(paste("There was a problem while importing your SAS file: '",e,"'"),"Import Error!",icon="error")
              })
          
        })
    csvdiscard <- gbutton("Discard ", handler=function(...){dispose(importDialog)})
    csvadjustTypes <- gbutton("Adjust Types", handler=typDialog)
    
    ftop <- gframe("Choose SAS-File:")
    gtop <- ggroup(horizontal=TRUE, container=ftop)
    add(ftop, filename, expand=TRUE)
    add(ftop, filebutton)
    layout[1,1:7] <- ftop
    layout[2,6, expand=FALSE] <- csvaccept
    layout[2,7, expand=FALSE] <- csvdiscard
    add(importDialogFrame, layout, expand=TRUE)
    buttonHandler()
  }
  
  # Data - Export - Export SAS
  exportSAS <- function(...){
    if(existd("sdcObject")  == FALSE){
      gmessage("There is no dataset loaded for export!", "No Dataset!",icon="warning")
    }
    else{
      importDialog <- gwindow("Export SAS", parent=window, width=100, height=100)
      putd("importDialog",importDialog)
      putd("dframe", NULL)
      importDialogFrame <- ggroup(container=importDialog, horizontal=FALSE)
      layout <- glayout()
      datafilename <- gedit()
      codefilename <- gedit()
      enabled(datafilename) <- FALSE
      enabled(codefilename) <- FALSE
      edit.dataname <- gedit("rdata")
      combo.validvarname <- gcombobox(c("<=6",">=7"), selected=2)
      
      databuttonHandler <- function(...){
        gfile(text = "Save Data File", type = "save", handler=function(h,...){
              if(grepl("^.*\\.(dat)$", h$file)){
                svalue(datafilename) <- h$file
              }
              else{
                svalue(datafilename) <- paste(h$file, ".dat", sep="")
              }
            })
      }
      
      codebuttonHandler <- function(...){
        gfile(text = "Save SAS File", type = "save", filter=list(".sas"=list("*.sas")),handler=function(h,...){
              if(grepl("^.*\\.(sas|SAS)$", h$file)){
                svalue(codefilename) <- h$file
              }
              else{
                svalue(codefilename) <- paste(h$file, ".sas", sep="")
              }
            })
      }
      
      #setup sas export gui
      datafilebutton <- gbutton("...", handler=databuttonHandler)
      codefilebutton <- gbutton("...", handler=codebuttonHandler)
      csvaccept <- gbutton("Accept", handler=function(...){
            tryCatch({version <- paste("V",substr(svalue(combo.validvarname), 3,3), sep="")
                  write.foreign(sdcGUIoutput(), datafile=svalue(datafilename),
                      codefile=svalue(codefilename),
                      package = "SAS",
                      dataname = svalue(edit.dataname),
                      validvarname = version)
                  putd("exportFileName", svalue(datafilename))
                  if(svalue(radio.html, index=TRUE)==2)
                    exportReport()
                },
                error=function(e){gmessage(paste("There was a problem while exporting your data: '",e,"'"), "Problem",
                      icon="error")})
            dispose(importDialog)
          })
      csvdiscard <- gbutton("Discard ", handler=function(...){dispose(importDialog)})
      
      #record export
      frame.html <- gframe("Generate report?")
      radio.html <- gradio(c("no", "yes"), 
          horizontal=TRUE, container=frame.html)
      
      fdata <- gframe("Choose Data-File (Contains exported data as freetext):")
      gdata <- ggroup(horizontal=TRUE, container=fdata)
      add(fdata, datafilename, expand=TRUE)
      add(fdata, datafilebutton)
      layout[1,1:7] <- fdata
      
      fcode <- gframe("Choose Code-File (Contains SAS Code for import):")
      gcode <- ggroup(horizontal=TRUE, container=fcode)
      add(fcode, codefilename, expand=TRUE)
      add(fcode, codefilebutton)
      layout[2,1:7] <- fcode
      
      fparams <- gframe("SAS-Parameters:")
      glayout <- glayout(container=fparams)
      glayout[1,1, anchor=c(-1,0)] <- glabel("future SAS data set name:")
      glayout[1,2, expand=FALSE] <- edit.dataname
      glayout[2,1, anchor=c(-1,0)] <- glabel("SAS version :")
      glayout[2,2, expand=FALSE] <- combo.validvarname
      layout[3:4, 1:7] <- fparams
      layout[5, 1:7] <- frame.html
      layout[6,6, expand=FALSE] <- csvaccept
      layout[6,7, expand=FALSE] <- csvdiscard
      add(importDialogFrame, layout, expand=TRUE)
      #add(importDialogFrame, statusbar)
      
    }
    
  }
  
  #outdir is the name of the exported data file from the different export dialogs
  exportReport <- function(...){
    if(existd("sdcObject")){
      reportDialog <- gwindow("Generate Report", parent=window, width=400, height=300)
      reportDialogG <- ggroup(container=reportDialog, horizontal=FALSE)
      pramRadio <- gcheckbox("Pram", checked=TRUE)
      kAnonRadio <- gcheckbox("k-Anonymity", checked=TRUE)
      indivRiskRadio <- gcheckbox("Individual Risk", checked=TRUE)
      hierRiskRadio <- gcheckbox("Hierachical Risk", checked=TRUE)
      riskNumKeyVarsRadio <- gcheckbox("Risk of Numerical Key Variables", checked=TRUE)
      dataUtilityRadio <- gcheckbox("Data Utility", checked=TRUE)
      localSuppsRadio <- gcheckbox("Local Supression", checked=TRUE)
      dataUtilityContRadio <- gcheckbox("Data Utility Cont", checked=TRUE)
      sessionInfoRadio <- gcheckbox("R Session Info", checked=TRUE)
      repTitle <- gedit(width=100)
      svalue(repTitle) <- "SDC-Report"
      reportDialogTitleFrame <- gframe("Title:", container=reportDialogG, horizontal=FALSE)
      add(reportDialogTitleFrame,repTitle)
      reportDialogFrame <- gframe("Content Types:", container=reportDialogG, horizontal=FALSE)
      add(reportDialogFrame,pramRadio)
      add(reportDialogFrame,kAnonRadio)
      add(reportDialogFrame,indivRiskRadio)
      add(reportDialogFrame,hierRiskRadio)
      add(reportDialogFrame,riskNumKeyVarsRadio)
      add(reportDialogFrame,dataUtilityRadio)
      add(reportDialogFrame,localSuppsRadio)
      add(reportDialogFrame,dataUtilityContRadio)
      add(reportDialogFrame,sessionInfoRadio)
      reportDialogOutFrame <- gframe("Output Type:", container=reportDialogG, horizontal=FALSE)
      outputRadio <- gradio(c("HTML", "LATEX", "TEXT"), 
          horizontal=TRUE, container=reportDialogOutFrame)
      okbutton <- gbutton("OK", container=reportDialogG, handler=function(h,...){
            obj <- ActiveSdcObject()
            obj@options$cmd <- getd("activeScript")$cmd
            if(existd("exportFileName")){
              exportFileName <- getd("exportFileName")
            }else{
              if(svalue(outputRadio)=="LATEX") {
                exportFileName <- gfile("Select file to save report to", parent=window, type="save" ,filter=list("LATEX"=list(patterns=c("*.tex")), "All files" = list(patterns = c("*"))))
              } 
              if (svalue(outputRadio)=="HTML") {
                exportFileName <- gfile("Select file to save report to", parent=window, type="save" ,filter=list("HTML"=list(patterns=c("*.html", "*.htm")), "All files" = list(patterns = c("*"))))
              }
              if (svalue(outputRadio)=="TEXT") {
                exportFileName <- gfile("Select file to save report to", parent=window, type="save" ,filter=list("TEXT"=list(patterns=c("*.txt")), "All files" = list(patterns = c("*"))))
              }
            }
            outdir <- dirname(exportFileName)
            filename <- strsplit(basename(exportFileName),"\\.")[[1]][1]
            if(svalue(outputRadio)=="LATEX") {
              filename <- paste(filename,".tex",sep="")
            }
            if (svalue(outputRadio)=="HTML") {
              filename <- paste(filename,".html",sep="")
            }
            if (svalue(outputRadio)=="TEXT") {
              filename <- paste(filename,".txt",sep="")
            }

            report(obj,
              pram=svalue(pramRadio),
              kAnon=svalue(kAnonRadio),
              indivRisk=svalue(indivRiskRadio),
              hierRisk=svalue(hierRiskRadio),
              riskNumKeyVars=svalue(riskNumKeyVarsRadio),
              dataUtility=svalue(dataUtilityRadio),
              localSupps=svalue(localSuppsRadio),
              dataUtilityCont=svalue(dataUtilityContRadio),
              sessionInfo=svalue(sessionInfoRadio),outdir=outdir,filename=filename,title=svalue(repTitle),
              format=svalue(outputRadio))
            dispose(reportDialog)
          })
      
      
    }else{
      gmessage("No sdc object found to generate report.", title="Information", icon="warning", parent=window)
    }
  }
  
  # Waiting Dialog
  WaitingDialog <- function(parent, text="<b><big>Importing Data, Please Wait!</big></b>", 
      header="Importing!", Parent=NULL){
    window <- gwindow(header, parent=Parent, width=100, height=50)
    glabel(text, markup=TRUE,container=window)
    return(window)
  }
  
  # Script - New Script
  newScript <- function(...) {
    ns_do <- gconfirm("A new script will be started.\nAre you sure?", title="Information",
        icon="warning", parent=window)
    if( ns_do ) {
      Script.new()
    }
  }
  
  # Script - Save Script
  saveScript <- function(...) {
    saveScriptToFile <- function(fileName, ...) {
      cmdtmp <- Script()$cmd
      if(length(grep("sdcMicroScript",fileName))==0)
        fileName <- paste(fileName,".sdcMicroScript", sep="")
      fo <- file(fileName)
      writeLines(cmdtmp,fo)
      close(fo)
    }
    if( existd("activeScript") ) {
      xname <- gfile("Select file to save Script", type="save", parent=window,
          filter=list("Script files" = list(patterns = c("*.sdcMicroScript")),"All files" = list(patterns = c("*.*"))))
      
      if( xname != "" ) {
        saveScriptToFile(xname)
      }
    } else {
      gmessage("No active Script found.", title="Information", icon="warning",
          parent=window)
    }
  }
  
  # Script - Load Script
  loadScript <- function(...) {
    # open file browser and load the needed script
    xname <- gfile("Select script file to open.", parent=window, type="open", 
        filter=
            
            list("Script files" = list(patterns = c("*.sdcMicroScript")),"All files" = list(patterns = c("*.*")))) 
    if( xname != '' ) {
      fo <- file(xname)
      cmdtmp <- list(cmd=readLines(fo))
      close(fo)
      Script.new()
      putd("activeScript", cmdtmp)
      Script.run()
    }
  }
  
  # Script - View Script
  # TODO: implement view script
  viewScript <- function(...) {
    cmdhist <- Script()$cmd
    if( is.null(cmdhist) ) {
      gmessage("No script present at the moment.", title="Attention", icon="warning", parent=window)
    } else {
      sureQuit <- function(...) {
        gconfirm("Do you want to close the window without saving?", icon="question", parent=scriptEditWindow,
            handler=function(h,...) quitEditScriptWindow() )
      }
      quitEditScriptWindow <- function(...) {
        xtmp <- list(cmd=c(xscript[]))
        Script(xtmp)
        dispose(scriptEditWindow)
      }
      runCMDhist <- function(...) {
        rto <- as.numeric(svalue(runTo))
        cmdhist <- xscript[]
        if( is.numeric(rto) & !is.na(rto) ) {
          if( rto>0 & rto<(length(cmdhist)+1) ) {
            cmdhisttmp <- cmdhist[c(1:rto)]
            Script.run(cmdhisttmp)
            quitEditScriptWindow()
          }
        } else {
          gmessage("Script step not valid.", title="Input not valid", icon="info", parent=scriptEditWindow)
        }
      }
      delCMDhist <- function(...) {
        dto <- as.numeric(svalue(delRow))
        cmdhist <- xscript[]
        if( is.numeric(dto) & !is.na(dto) ) {
          if( dto>0 & dto<(length(cmdhist)+1) ) {
            cmdhisttmp <- cmdhist[-dto]
            xscript[] <- cmdhisttmp
            svalue(delRow) <- ""
          }
        } else {
          gmessage("Script step not valid.", title="Input not valid", icon="info", parent=scriptEditWindow)
        }
      }
      scriptEditWindow = gwindow("View script", parent=window, width=700, height=400)
      scriptWidget = ggroup(horizontal=FALSE)
      xscript = gdf(cmdhist, expand=TRUE)
      # TODO: find replacement, cause in linux it wouldnt display anything.
      #enabled(xscript) <- FALSE
      add(scriptWidget, xscript, expand=TRUE)
      gseparator(container=scriptWidget)
      saveCancelGroup = ggroup(container=scriptWidget)
      addSpring(saveCancelGroup)
      tmp = ggroup(container=saveCancelGroup)
      glabel("Delete script step: ", container=tmp)
      delRow = gedit(text="", width=3, container=tmp)
      gbutton("Delete", container=tmp, handler=function(h,...) delCMDhist() )
      addSpring(saveCancelGroup)
      tmp = ggroup(container=saveCancelGroup)
      glabel("Run script to row: ", container=tmp)
      runTo = gedit(text=length(cmdhist), width=3, container=tmp)
      gbutton("Run", container=tmp, handler=function(h,...) runCMDhist() )
      addSpring(saveCancelGroup)
      gbutton("Close", container=saveCancelGroup, handler=function(h,...) quitEditScriptWindow() )
      
      add(scriptEditWindow, scriptWidget)
    }
  }
  
  # Script - Run Script
  runScript <- function(...) {
    # dialog and ask if you want to run the whole script on this dataset
    Script.run()
  }
  
  # GUI - Quit
  quitGUI <- function(...) {
    val <- gconfirm("Do you really want to close the window?", parent=window)
    if( as.logical(val) ) {
      dispose(window)
      if(!is.null(options("quitRwithsdcGUI")[[1]])){#if started with custom binary windows build, quit R toos
        cat("quitting R now\n")
        quit("no")
      }
    }
  }
  OneStepBack <- function(...) {
    if(existd("sdcObject")){
      if(!is.null(getd("sdcObject")@prev)){
        acs <- getd("activeScript")$cmd
        cmd <- acs[[length(acs)]]
        val <- gconfirm(paste("Do you really want to undo the last command:\n",cmd,sep=""), parent=window)
        if( as.logical(val) ) {
          putd("activeScript",list(cmd=acs[-length(acs)]))
          ActiveSdcObject(undolast(ActiveSdcObject()))
          freqCalcIndivRisk()
          nm_risk_print_function()
        }
      }else
        gmessage("Undo is not possible, because no previous sdc object was found.", title="Attention", icon="error", parent=window)
  }else
      gmessage("Undo is not possible, because no active sdc object was found.", title="Attention", icon="error", parent=window)
  }
  restartGUI <- function(...) {
    val <- gconfirm("Do you really want to delete everything and restart the GUI?", parent=window)
    if( as.logical(val) ) {
      dispose(window)
      #rm(list=ls())
      rmd(listd())
      sdcGUI()
    }
  }
  
  ## initialize
  # set first run
  putd("firstRun", TRUE)
  # set up new script
  Script.new()
  # get values of internal vars if they exist
  activeDataSet <- if( existd("activeDataSet") ) getd("activeDataSet") else ""
  dataSetName <- if( existd("dataSetName") ) getd("dataSetName") else ""
  # save intitial values in env
  if( !dataSetName=="" ) {
    ActiveDataSet(dataSetName)
  }
  putd("dataSetName", dataSetName)
  
  #putd("importFileName", "No File imported!")
  
  ## create window
  window = gwindow("sdcMicro GUI")
  addHandlerUnrealize(window, handler = function(h,...) {
        val <- gconfirm("Do you really want to close the window?", parent=h$obj)
        if(as.logical(val))
          return(FALSE)             # destroy
        else
          return(TRUE)              # don't destroy
      })
  
  ## Menubar
  mbar = list()
  mbar$GUI$Quit$handler = quitGUI
  mbar$GUI$Restart$handler = restartGUI
  mbar$GUI$"Check for Updates"$handler = updates22 <- function(...)updates2(restart=TRUE)
  mbar$Data$"Load R-Dataset"$handler = loadDataSet
  mbar$Data$"Choose R-Dataset"$handler = setDataSet
  mbar$Data$"Save Dataset to"$File$handler = saveToFile
  mbar$Data$"Save Dataset to"$"R Object"$handler = saveToVariable
  #TODO: change handler
  mbar$Data$Import$"Import CSV"$handler = importCSV
  mbar$Data$Import$"Import SPSS"$handler = importSPSS
  mbar$Data$Import$"Import SAS"$handler = importSAS
  mbar$Data$Import$"Import STATA"$handler = loadSTATA
  mbar$Data$Export$"Export CSV"$handler = exportCSV
  mbar$Data$Export$"Export SPSS"$handler = exportSPSS
  mbar$Data$Export$"Export SAS"$handler = exportSAS
  mbar$Data$Export$"Export STATA"$handler = exportSTATA
  mbar$Data$"Generate Report"$handler = exportReport
  #mbar$Data$"Generate Report"$"Full report (for internal use)"$handler=function(...)exportReport(version=2)
  #mbar$Data$"Generate Report"$"Short report (for external use)"$handler=function(...)exportReport(version=3)
  #mbar$Script$"New"$handler = newScript
  mbar$Script$"Import"$handler = loadScript
  mbar$Script$"Export"$handler = saveScript
  mbar$Script$"View"$handler = viewScript
#  mbar$Script$"Run"$handler = runScript
  mbar$Help$"GUI-Guidelines"$handler = vign
  mbar$Help$"GUI-index"$"Risk (categorical)"$handler=function(...)helpR("measure_risk")
  mbar$Help$"GUI-index"$"Global Recode"$handler=function(...)helpR("globalRecode")
  mbar$Help$"GUI-index"$"Pram"$handler=function(...)helpR("pram_strata")
  mbar$Help$"GUI-index"$"Local Supression (optimal - k-Anonymity)"$handler=function(...)helpR("localSuppression")
  mbar$Help$"GUI-index"$"Local Supression (threshold - indiv.risk)"$handler=function(...)helpR("localSupp")
  mbar$Help$"GUI-index"$"Risk (continuous)"$handler=function(...)helpR("dRisk")
  mbar$Help$"GUI-index"$"Mircoaggregation"$handler=function(...)helpR("microaggregation")
  mbar$Help$"GUI-index"$"Add Noise"$handler=function(...)helpR("addNoise")
  mbar$Help$"GUI-index"$"Shuffling"$handler=function(...)helpR("shuffle")
  mbar$Help$"GUI-index"$"Data Utility (continuous)"$handler=function(...)helpR("dUtility")
  mbar$Help$"Package Index"$handler = paind
  mbar$Undo$"Undo last action"$handler=OneStepBack
  
  
  ## layout
  mainGroupX = ggroup(container=window, horizontal=FALSE)
  # Start - add menu
  add(mainGroupX, gmenu(mbar))
  nbMain <- gnotebook(container=mainGroupX, closebuttons=FALSE)
  mainGroup = ggroup(container=nbMain, horizontal=FALSE,label="Identifiers")
  mainGroupCat = ggroup(container=nbMain, horizontal=TRUE,label="Categorical")
  mainGroupCont = ggroup(container=nbMain, horizontal=TRUE,label="continuous")
  svalue(nbMain) <- 1
  # End - add menu
  # Start - variable Selection Container
  varSelGroup = ggroup(container=mainGroup, horizontal=FALSE)
  
  varSelGroupButton = ggroup(container=varSelGroup, horizontal=FALSE)
  glabel("Loaded data set:", container=varSelGroupButton)
  if( existd("dataSetName") ) {
    if(getd("dataSetName")!=""){
      dslab = glabel(paste(getd("dataSetName")," (n=",nrow(ActiveDataSet()),")",sep=""),container=varSelGroupButton)
    }else
      dslab = glabel("none",container=varSelGroupButton)
  } else {
    dslab = glabel("none",container=varSelGroupButton)
  }
  gb1 = gbutton(text="Select key variables / Reset", container=varSelGroupButton,
      handler=function(h,...) confirmSelection())
  tooltip(gb1) <- "(Re)-identify categorical, numerical variables (and the weight variable, the household ID variable and the strata variables)"
  enabled(gb1) <- FALSE
  
  gb2 = gbutton(text="Remove direct identifiers", container=varSelGroupButton,
      handler=function(h,...) removeDirectID_menu())
  tooltip(gb2) <- "Remove variables which can be used as direct identifiers."
  enabled(gb2) <- FALSE
  
  
  
  mtmp = ggroup(container=varSelGroup, horizontal=FALSE, expand=TRUE)
  tmp = gframe("Selected key variables", container=mtmp, horizontal=FALSE)
  tmpCat = gframe("Categorical", container=tmp, horizontal=FALSE,pos=.3)
  tab1 = glabel("not selected\n")#categorical info
  tooltip(tab1) <- tt_selVar
  add(tmpCat, tab1, expand=TRUE)
  addSpace(tmp, 1)
  tmpNum = gframe("Numerical", container=tmp, horizontal=FALSE,pos=.3)
   tab2 <- glabel("not selected\n")#numerical info
  tooltip(tab2) <- tt_selVar
  add(tmpNum, tab2, expand=TRUE)
  addSpace(mtmp, 4,horizontal=FALSE)
  tmp = gframe("Selected auxiliary variables", container=mtmp, horizontal=FALSE)
  tmpW = gframe("Weight", container=tmp, horizontal=FALSE,pos=.3)
  tab3 <- glabel("not selected\n")#weight info
  tooltip(tab3) <- tt_selVar
  add(tmpW, tab3, expand=TRUE)
  addSpace(tmp, 1)
  tmpHH = gframe("Household ID", container=tmp, horizontal=FALSE,pos=.3)
  tab4 <- glabel("not selected\n")# household info
  tooltip(tab4) <- tt_selVar
  add(tmpHH, tab4, expand=TRUE)
  addSpace(tmp, 1)
  tmpSt = gframe("Strata", container=tmp, horizontal=FALSE,pos=.3)
  tab5 <- glabel("not selected\n")#strata info
  tooltip(tab5) <- tt_selVar
  add(tmpSt, tab5, expand=TRUE)
  

  
  # End - variable Selection Container
  # Start - Categorical Container
  tmpCR = gframe("Risk", container=mainGroupCat, horizontal=FALSE)
  tmpCP = gframe("Protection", container=mainGroupCat, horizontal=FALSE)
  tmpCU = gframe("Information Loss", container=mainGroupCat, horizontal=FALSE)
   
  fc_tmp = gframe("Frequency calculations",
      container=tmpCR, expand=TRUE)
  #tmp = gframe("(Individual) risk computation", container=fc_tmp)
  tmp = gframe("", container=fc_tmp,horizontal=FALSE)
  fc_print = gtext(text="", width=280, height=150)
  tooltip(fc_print)<- tt_print
  putd("fc_print",fc_print)
  add(tmp, fc_print)
  #addSpring(fc_tmp)
  vkGroupButton = ggroup(container=tmp,horizontal=FALSE)
  addSpring(vkGroupButton)
  vk_button = gbutton("View Observations violating 3-anonymity", container=vkGroupButton,
      handler=function(h, ...) viewkanon())
  enabled(vk_button) <- FALSE
   
  
  ir_tmp = gframe("Risk for categorical key variables", container=tmpCR, horizontal=FALSE)
  ir_print = gtext(text="", width=280, height=190)
  tooltip(ir_print)<- tt_ir
  add(ir_tmp, ir_print)

  vh_button = gbutton("View Observations with high risk", container=ir_tmp,
      handler=function(h, ...) viewhigh())
  enabled(vh_button) <- FALSE
  tooltip(vh_button) <- "Show 20 observations with highest risk"

  # Start - l-Diversity Container
  ld_button1 = gbutton("l-Diversity",
      handler=function(h,...) ldiv1() )
  tooltip(ld_button1) <- tt_ld1
  add(ir_tmp, ld_button1)
  enabled(ld_button1) <- FALSE
  
  tmp = gframe("Recodings", container=tmpCU)
  recode_summary = gtext(text="", width=240, height=260)
  tooltip(recode_summary)<- "Recoded values"
  add(tmp, recode_summary)
  
  tmp = gframe("Suppressions", container=tmpCU)
  fc_summary = gtext(text="", width=240, height=180)
  tooltip(fc_summary)<- "Supressed values"
  add(tmp, fc_summary)
  
  
  
  # End - indivRisk Container
  # Start - globalRecode Container
  vc_button1 = gbutton("Recode", handler=function(h, ...) vc() )
  tooltip(vc_button1) <- tt_vc
  enabled(vc_button1) <- FALSE
  add(tmpCP, vc_button1)
  #globalRecodeGroupRight = ggroup(container=tmp, horizontal=FALSE)
  #tmp = gframe("Experts only", container=globalRecodeGroupRight)
  #gr_button2 = gbutton("Manual R commands", handler=function(h, ...) scriptWindow() )
  #enabled(gr_button2) <- FALSE
  #add(tmp, gr_button2)
  # End - globalRecode Container
  # Start - pram Container
  pram_button1 = gbutton("pram",
      handler=function(h,...) pram1() )
  tooltip(pram_button1) <- tt_pram1
  add(tmpCP, pram_button1)
  enabled(pram_button1) <- FALSE
  
  # Start - localSupp Container
  tmp = gframe("Local suppression", container=tmpCP, horizontal=FALSE)
  ls_button1 = gbutton("optimal (k-Anonymity)",
      handler=function(h,...) ls4() )
  tooltip(ls_button1) <- tt_ls1
  enabled(ls_button1) <- FALSE
  add(tmp, ls_button1)
  ir_button = gbutton("threshold (indiv.Risk))", 
      handler=function(h, ...) plotIndivRisk())
  add(tmp, ir_button)
  tooltip(ir_button) <- tt_pir
  enabled(ir_button) <- FALSE

  
  
  #globalRecodeGroupRight = ggroup(container=globalRecodeGroup, horizontal=FALSE)
  #tmp = gframe("Experts only", container=globalRecodeGroupRight)
  #gr_button2 = gbutton("Manual R commands", handler=function(h, ...) scriptWindow() )
  #tooltip(gr_button2) <- tt_man
  #enabled(gr_button2) <- FALSE
  #add(tmp, gr_button2)
  #addSpring(tmp)
  
  
  # End - localSupp Container
  # Start - Continous Container
  tmpR = gframe("Risk", container=mainGroupCont,horizontal=FALSE)
  tmpP = gframe("Protection", container=mainGroupCont,horizontal=FALSE)
  tmpU = gframe("Information Loss", container=mainGroupCont,horizontal=FALSE)
  tmp1 = ggroup(container=tmp, horizontal=FALSE)
  addSpring(tmp1)
  
  nm_button2 = gbutton("Microaggregation", handler=function(h,...) nm2() )
  tooltip(nm_button2) <- tt_ma
  add(tmpP, nm_button2)
  enabled(nm_button2) <- FALSE
  
  nm_button1 = gbutton("Add noise", handler=function(h,...) nm1() )
  tooltip(nm_button1) <- tt_noi
  add(tmpP, nm_button1)
  enabled(nm_button1) <- FALSE
  

  
  shuffle_button1 = gbutton("Shuffling", handler=function(h,...) shuffle1() )
  tooltip(shuffle_button1) <- tt_shuffle
  add(tmpP, shuffle_button1)
  enabled(shuffle_button1) <- FALSE
  
  
#  addSpring(tmp1)
#  nm_button3 = gbutton("Recalculate risk", handler=function(h,...) nm_risk_print_function() )
#  tooltip(nm_button3) <- tt_rr
#  add(tmp1, nm_button3)
#  enabled(nm_button3) <- FALSE
#  addSpring(tmp1)
#  tmp1 = ggroup(container=tmp, horizontal=FALSE, expand=TRUE)
#  tmp2 = gframe("Parameters for risk est.", container=tmp1, horizontal=FALSE)
#  tmp3 = ggroup(container=tmp2)
#  glabel("k ", container=tmp3)
#  nm_risk_slider1 = gslider(from=0, to=0.1, by=0.01, value=0.01)
#  tooltip(nm_risk_slider1) <- tt_slider1
#  enabled(nm_risk_slider1) = FALSE
#  add(tmp3, nm_risk_slider1, expand=TRUE)
#  tmp3 = ggroup(container=tmp2)
#  glabel("k2", container=tmp3)
#  nm_risk_slider2 = gslider(from=0, to=0.05, by=0.01, value=0.05)
#  tooltip(nm_risk_slider2) <- tt_slider2
#  enabled(nm_risk_slider2) = FALSE
#  add(tmp3, nm_risk_slider2, expand=TRUE)
#  tmp1 = ggroup(container=tmp)
#  tmp2 = gframe("Risk/Utility for continuous key variables", container=tmp1)
  nm_risk_print = gtext(text="", width=280, height=480)
  tooltip(nm_risk_print) <- tt_nmr
  add(tmpR, nm_risk_print)
  nm_util_print = gtext(text="", width=280, height=480)
  tooltip(nm_util_print) <- tt_nmr
  add(tmpU, nm_util_print)
  # End - numericalMethod Container
}
# TODO: remove for final version
#sdcGUI()
