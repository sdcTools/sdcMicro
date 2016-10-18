library(shiny)

shinyServer(function(session, input, output) {
  for (file in list.files("controllers")) {
    source(file.path("controllers", file), local=TRUE)
  }
  values <- reactiveValues(starting = TRUE)
  session$onFlushed(function() {
    values$starting <- FALSE
  })

  autoInvalidate <- reactiveTimer(25000)
  # after 25 seconds, the reactive-values used to display the
  # confirm-buttons for resetting the sdcObj and the microdata
  # are resetted
  # The output$functions are updating every 15 seconds to check,
  # if something has changed
  observe({
    autoInvalidate()
    obj$reset_inputdata1 <- 0
    obj$reset_sdc1 <- 0
  })

  # dynamically generate inputs (currently used in setup-sdcProblem)
  shinyInput <- function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }

  observe({
    curObj <- sdcObj()
    if (!is.null(curObj)) {
      pramInfo <- curObj@pram
      x <- print(curObj, type="pram", docat=FALSE)

      if (!is.null(x)) {
        dt <- x$pram_summary
        for (i in 1:nrow(dt)){
          local({
            my_i <- i
            tablename <- paste0("transmat_pram_", my_i)
            output[[tablename]] <- renderTable({x$params[[my_i]]$Rs})
          })
        }
      }
    }
  })

  # obtain the values of inputs
  shinyValue <- function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }

  # reactive expression, we use this to change the output of output$sdcObj_exists in controllers/ui_setup_sdc.R
  lastWarning <- reactive({
    return(obj$last_warning)
  })
  # reactive expression, we use this to change the output of output$sdcObj_exists in controllers/ui_setup_sdc.R
  lastError <- reactive({
    return(obj$last_error)
  })

  ### START CODE GENERATION EXPRESSIONS ####
  # reactive expressions for code_generation

  # 1: modifications for microdata
  # code to read in microdata
  code_useRObj <- reactive({
    cmd <- paste0("inputdata <- readMicrodata(")
    cmd <- paste0(cmd, "path=",dQuote(input$sel_choose_df))
    cmd <- paste0(cmd, ", type=",dQuote("rdf"))
    cmd <- paste0(cmd, ", convertCharToFac=FALSE")
    cmd <- paste0(cmd, ", drop_all_missings=FALSE)")
    cmd
  })

  # read microdata
  code_readMicrodata <- reactive({
    cmd <- paste0("res <- readMicrodata(")
    cmd <- paste0(cmd, "path=",dQuote(normalizePath(input$file1$datapath,winslash = "/")))
    cmd <- paste0(cmd, ", type=",dQuote(input$dat_type))
    cmd <- paste0(cmd, ", convertCharToFac=",input$rb_convert_c_to_f)
    cmd <- paste0(cmd, ", drop_all_missings=",input$rb_drop_all_missings)
    if (input$dat_type=="csv") {
      cmd <- paste0(cmd, ", header=",input$import_csv_header)
      cmd <- paste0(cmd, ", sep=",dQuote(input$import_csv_sep))
    }
    cmd <- paste0(cmd,")")
    cmd
  })

  # code to import previously saved sdcProblem
  code_import_problem <- reactive({
    cmd <- paste0("res <- importProblem(path=",dQuote(normalizePath(input$file_importProblem$datapath,winslash = "/")),")")
    cmd
  })

  # code to reset variables in the input data set
  code_resetmicrovar <- reactive({
    cmd <- paste0("inputdata[, ",VecToRStr(input$sel_reset_microvars, quoted=TRUE),"] <- inputdataB[,",VecToRStr(input$sel_reset_microvars, quoted=TRUE),"]")
    cmd
  })

  # code to group or rename factor-levels
  code_groupAndRename <- reactive({
    cmd <- paste0("inputdata <- groupAndRename(obj=inputdata")
    cmd <- paste0(cmd, ", var=",dQuote(input$sel_factor))
    cmd <- paste0(cmd, ", before=",VecToRStr(input$cbg_factor, quoted=TRUE))
    cmd <- paste0(cmd, ", after=",VecToRStr(input$inp_newlevname, quoted=TRUE),")")
    cmd
  })

  # code to use only parts of the original microdata
  code_subsetMicrodata <- reactive({
    cmd <- paste0("inputdata <- sdcMicro:::subsetMicrodata(obj=inputdata, type=",dQuote(input$sel_sdcP_sample_type))
    cmd <- paste0(cmd, ", n=",input$sel_sdcP_sample_n,")")
    cmd
  })

  # creating factors from numeric variables for input data
  code_globalRecodeMicrodata <- reactive({
    if (input$sel_custom_split=="no") {
      cmd <- paste0("inputdata <- varToFactor(obj=inputdata")
      cmd <- paste0(cmd, ", var=",VecToRStr(input$sel_num_glrec, quoted=TRUE),")")
    } else {
      cmd <- paste0("inputdata <- globalRecode(obj=inputdata")
      cmd <- paste0(cmd, ", column=",dQuote(input$rb_num_glrec))
      if (input$sel_algo=="manual") {
        cmd <- paste0(cmd, ", breaks=",VecToRStr(input$txt_custom_breaks, quoted=FALSE))
      } else {
        cmd <- paste0(cmd, ", breaks=",VecToRStr(input$sl_number_breaks, quoted=FALSE))
        cmd <- paste0(cmd, ", method=",dQuote(input$sel_algo))
      }
      cmd <- paste0(cmd,")")
    }
    cmd
  })
  # creating numeric variables from factors or character variables
  code_globalRecodeMicrodataToNum <- reactive({
    cmd <- paste0("inputdata <- varToNumeric(obj=inputdata")
    cmd <- paste0(cmd, ", var=",VecToRStr(input$sel_to_num_var, quoted=TRUE),")")
    cmd
  })

  # set values to na in inputdata
  code_set_to_na <- reactive({
    if (input$set_to_na_type=="id") {
      cmd <- paste0("inputdata[",input$num_na_suppid,",",VecToRStr(input$sel_na_suppvar),"] <- NA")
    } else {
      if (is.numeric(obj$inputdata[[input$sel_na_suppvar]])) {
        cmd <- paste0("inputdata[which(inputdata$",input$sel_na_suppvar,"==",input$num_na_suppid,"), ",shQuote(input$sel_na_suppvar),"] <- NA")
      } else {
        cmd <- paste0("inputdata[which(inputdata$",input$sel_na_suppvar," ==",shQuote(input$num_na_suppid),"), ",shQuote(input$sel_na_suppvar),"] <- NA")
      }
    }
    cmd
  })

  # 2: anonymization methods
  # add linked (ghost)-vars to sdcMicroObj
  code_addGhostvars <- reactive({
    cmd <- paste0("sdcObj <- addGhostVars(sdcObj")
    cmd <- paste0(cmd, ", keyVar=",dQuote(input$sel_gv1))
    cmd <- paste0(cmd, ", ghostVars=",VecToRStr(input$sel_gv2, quoted=TRUE),")")
    cmd
  })

  # create a new randomized ID
  code_addRandID <- reactive({
    cmd <- paste0("sdcObj <- createNewID(sdcObj")
    cmd <- paste0(cmd, ", newID=",dQuote(input$txt_randid_newid))
    if (input$sel_randid_withinvar=="none") {
      cmd <- paste0(cmd, ", withinVar=NULL)")
    } else {
      cmd <- paste0(cmd, ", withinVar=",dQuote(input$sel_randid_withinvar),")")
    }
    cmd
  })

  # code for expert pram-application
  code_pram_expert <- reactive({
    v <- as.vector(as.matrix(obj$transmat))
    rn <- rownames(obj$transmat)
    matstr <- VecToRStr(v, quoted=FALSE)
    cmd <- paste0("mat <- matrix(",matstr,",ncol=",ncol(obj$transmat),"); ")
    cmd <- paste0(cmd,"rownames(mat) <- colnames(mat) <- ", VecToRStr(rn, quoted=TRUE),";\n")
    cmd <- paste0(cmd,"sdcObj <- pram(sdcObj, variables=",dQuote(input$sel_pramvars),", pd=mat)")
    cmd
  })

  # code for non-expert pram-application
  code_pram_nonexpert <- reactive({
    cmd <- paste0("sdcObj <- pram(sdcObj, variables=",VecToRStr(input$sel_pramvars, quoted=TRUE))
    cmd <- paste0(cmd,", pd=",input$sl_pd)
    cmd <- paste0(cmd,", alpha=",input$sl_alpha,")")
    cmd
  })

  # code for k-Anonymity
  code_kAnon <- reactive({
    cmd <- paste0("sdcObj <- kAnon(sdcObj")
    if (kAnon_useImportance()) {
      cmd <- paste0(cmd,", importance=",VecToRStr(kAnon_impvec(),quoted=FALSE))
    } else {
      # default: 1:n
      cmd <- paste0(cmd,", importance=",VecToRStr(1:length(get_keyVars()),quoted=FALSE))
    }

    if (input$rb_kanon_useCombs=="Yes") {
      params <- kAnon_comb_params()
      cmd <- paste0(cmd,", combs=",VecToRStr(params$use, quoted=FALSE))
      k <- params$k
    } else {
      cmd <- paste0(cmd, ", combs=NULL")
      k <- input$sl_kanon_k
    }
    cmd <- paste0(cmd, ", k=",VecToRStr(k, quoted=FALSE),")")
    cmd
  })

  # code to group or a key variable (factor)
  code_groupAndRename_keyvar <- reactive({
    cmd <- paste0("sdcObj <- groupAndRename(obj=sdcObj")
    cmd <- paste0(cmd, ", var=",dQuote(input$sel_recfac))
    cmd <- paste0(cmd, ", before=",VecToRStr(input$cbg_recfac, quoted=TRUE))
    cmd <- paste0(cmd, ", after=",VecToRStr(input$inp_newlevname_rec, quoted=TRUE),")")
    cmd
  })

  # code for local suppression with threshold
  code_suppThreshold <- reactive({
    cmd <- paste0("sdcObj <- localSupp(sdcObj")
    cmd <- paste0(cmd,", threshold=",input$sl_supp_threshold)
    cmd <- paste0(cmd,", keyVar=",dQuote(input$sel_supp_th_var),")")
    cmd
  })

  # code to generate a stratification-variable
  code_generateStrata <- reactive({
    cmd <- paste0("inputdata <- generateStrata(df=inputdata")
    cmd <- paste0(cmd, ", stratavars=",VecToRStr(input$sel_allvars_strata, quoted=TRUE))
    cmd <- paste0(cmd,", name=",dQuote(input$inp_vname_strata),")")
    cmd
  })

  # code to generate an sdcMicroObj
  code_createSdcObj <- reactive({
    cmd_out <- list()
    cmd_out$recode_to_factors <- NULL
    cmd_out$create_strata <- NULL
    cmd_out$setup_sdc <- NULL

    vars <- allVars()
    nc <- length(vars)
    type <- dataTypes()
    vv <- obj$setupval_inc
    useAsKey <- shinyValue(paste0("setup_key_",vv,"_"), nc)
    useAsPram <- shinyValue(paste0("setup_pram_",vv,"_"), nc)
    useAsWeight <- shinyValue(paste0("setup_weight_",vv,"_"), nc)
    useAsClusterID <- shinyValue(paste0("setup_cluster_",vv,"_"), nc)
    deleteVariable <- shinyValue(paste0("setup_remove_",vv,"_"), nc)
    useAsStrata <- shinyValue(paste0("setup_strata_",vv,"_"), nc)

    ind_kv <- which(useAsKey=="Cat.")
    kV <- vars[ind_kv]
    ii <- which(type[ind_kv]%in%c("character","integer"))
    if (length(ii)>0) {
      # recode to factor
      cmd_out$recode_to_factors <- list()
      i <- 1
      for (vv in ii) {
        cmd_toFac <- paste0("inputdata <- varToFactor(obj=inputdata")
        cmd_toFac <- paste0(cmd_toFac, ", var=",dQuote(kV[vv]),")")
        cmd_out$recode_to_factors[[length(cmd_out$recode_to_factors)+1]] <- cmd_toFac
      }
    }

    # sampling weights
    ind_w <- which(useAsWeight==TRUE)
    if (length(ind_w)==1) {
      wV <- vars[ind_w]
    } else {
      wV <- NULL
    }

    # clusterID
    ind_h <- which(useAsClusterID==TRUE)
    if (length(ind_h)==1) {
      hhId <- vars[ind_h]
    } else {
      hhId <- NULL
    }

    # pram vars
    ind_p <- which(useAsPram==TRUE)
    if (length(ind_p)>0) {
      pV <- vars[ind_p]
    } else {
      pV <- NULL
    }

    # numeric key variables
    ind_nv <- which(useAsKey=="Cont.")
    if (length(ind_nv)>0) {
      nV <- vars[ind_nv]
    } else {
      nV <- NULL
    }

    # exclude variables
    ind_d <- which(deleteVariable==TRUE)
    if (length(ind_d)>0) {
      excludeVars <- vars[ind_d]
    } else {
      excludeVars <- NULL
    }

    # create stratification variable if more than 1 variable is listed!
    ind_s <- which(useAsStrata==TRUE)
    if (length(ind_s)==0) {
      strataVar <- NULL
    } else if (length(ind_s)==1) {
      strataVar <- vars[ind_s]
    } else {
      strataVar <- paste0(vars[ind_s], collapse="_")
      cmd_genStrata <- paste0("inputdata <- generateStrata(df=inputdata")
      cmd_genStrata <- paste0(cmd_genStrata, ", stratavars=",VecToRStr(vars[ind_s], quoted=TRUE))
      cmd_genStrata <- paste0(cmd_genStrata,", name=",dQuote(strataVar),")")
      cmd_out$create_strata <- cmd_genStrata
    }

    cmd <- paste0("obj$sdcObj <- createSdcObj(dat=obj$inputdata,")
    cmd <- paste0(cmd, "\n\tkeyVars=",VecToRStr(kV, quoted=TRUE))
    if (!is.null(nV)) {
      cmd <- paste0(cmd, ", \n\tnumVars=",VecToRStr(nV, quoted=TRUE))
    } else {
      cmd <- paste0(cmd, ", \n\tnumVars=NULL")
    }
    if (!is.null(wV)) {
      cmd <- paste0(cmd, ", \n\tweightVar=",VecToRStr(wV, quoted=TRUE))
    } else {
      cmd <- paste0(cmd, ", \n\tweightVar=NULL")
    }
    if (!is.null(hhId)) {
      cmd <- paste0(cmd, ", \n\thhId=",VecToRStr(hhId, quoted=TRUE))
    } else {
      cmd <- paste0(cmd, ", \n\thhId=NULL")
    }
    if (!is.null(strataVar)) {
      cmd <- paste0(cmd, ", \n\tstrataVar=",VecToRStr(strataVar, quoted=TRUE))
    } else {
      cmd <- paste0(cmd, ", \n\tstrataVar=NULL")
    }
    if (!is.null(pV)) {
      cmd <- paste0(cmd, ", \n\tpramVars=",VecToRStr(pV, quoted=TRUE))
    } else {
      cmd <- paste0(cmd, ", \n\tpramVars=NULL")
    }
    if (!is.null(excludeVars)) {
      cmd <- paste0(cmd, ", \n\texcludeVars=",VecToRStr(excludeVars, quoted=TRUE))
    } else {
      cmd <- paste0(cmd, ", \n\texcludeVars=NULL")
    }
    cmd <- paste0(cmd, ", \n\tseed=",input$sl_seed)
    cmd <- paste0(cmd, ", \n\trandomizeRecords=FALSE")
    cmd <- paste0(cmd, ", \n\talpha=",VecToRStr(input$sl_alpha, quoted=FALSE))
    cmd <- paste0(cmd,")")
    cmd_out$setup_sdc <- cmd
    cmd_out
  })

  # code to add ghost-vars to existing key variables
  code_addGhostVars <- reactive({
    cmd <- paste0("sdcObj <- addGhostVars(obj=sdcObj, keyVar=",dQuote(input$sel_gv1),", ghostVars=c(",dQuote(paste(input$sel_gv2, collapse='","')), "))")
    cmd
  })

  # code for top-bottom coding on sdcMicroObj
  code_topBotCoding_num <- reactive({
    cmd <- paste0("sdcObj <- topBotCoding(obj=sdcObj, column=",dQuote(input$sel_topbot_var_num))
    cmd <- paste0(cmd, ", value=",as.numeric(input$num_topbot_val_num))
    cmd <- paste0(cmd, ", replacement=",as.numeric(input$num_topbot_replacement_num))
    cmd <- paste0(cmd, ", kind=",dQuote(input$sel_topbot_kind_num),")")
    cmd
  })

  # code for microaggregation()
  code_microaggregation <- reactive({
    m_method <- input$sel_microagg_method
    cmd <- NULL
    cmd_reset_strata <- list(cmd1=NA, cmd2=NA)
    if (input$sel_microagg_strata!="usedefault") {
      exStrata <- get_strataVar_names()
      if (input$sel_microagg_strata=="none") {
        cmd_reset_strata <- code_reset_strata(new_strataV=NULL, ex_strataV=exStrata)
      } else {
        cmd_reset_strata <- code_reset_strata(new_strataV=input$sel_microagg_strata,
          ex_strataV=exStrata)
      }
    }
    cmd <- paste0("sdcObj <- microaggregation(obj=sdcObj")
    if (is.null(input$sel_microagg_v)) {
      cmd <- paste0(cmd, ", variables=NULL")
    } else {
      cmd <- paste0(cmd, ", variables=",VecToRStr(input$sel_microagg_v, quoted=TRUE))
    }
    cmd <- paste0(cmd, ", aggr=",input$sl_microagg_aggr)
    cmd <- paste0(cmd, ", method=",dQuote(m_method))
    if (!m_method %in% c("mdav","rmd")) {
      cmd <- paste0(cmd, ", measure=",dQuote(input$sl_microagg_measure))
      cmd <- paste0(cmd, ", trim=",input$sl_microagg_trim)
      if (m_method=="single") {
        vs <- match(input$sel_microagg_varsort, cn)
        cmd <- paste0(cmd, ", varsort=",vs)
      } else {
        cmd <- paste0(cmd, ", varsort=1")
      }
    }
    # parameters for cluster-based methods
    if (input$rb_microagg_cluster=="Yes") {
      cmd <- paste0(cmd, ", clustermethod=",dQuote(input$rb_microagg_clustermethod))
      cmd <- paste0(cmd, ", transf=",dQuote(input$rb_microagg_transf))
      cmd <- paste0(cmd, ", nc=",input$sl_microagg_nc)
    }
    cmd <- paste0(cmd,")")
    list(cmd=cmd, cmd_reset_strata=cmd_reset_strata)
  })

  # code for microaggregation()
  code_addNoise <- reactive({
    n_method <- input$sel_noise_method
    cmd <- paste0("sdcObj <- addNoise(obj=sdcObj")
    if (is.null(input$sel_noise_v)) {
      cmd <- paste0(cmd, ", variables=NULL")
    } else {
      cmd <- paste0(cmd, ", variables=",VecToRStr(input$sel_noise_v, quoted=TRUE))
    }

    if (n_method =="correlated2") {
      cmd <- paste0(cmd, ", delta=",VecToRStr(input$sl_noise_noise, quoted=FALSE))
      cmd <- paste0(cmd, ", noise=NA, p=NA")
    } else if (n_method=="ROMM") {
      cmd <- paste0(cmd, ", p=",VecToRStr(input$sl_noise_noise, quoted=FALSE))
      cmd <- paste0(cmd, ", noise=NA, delta=NA")
    } else {
      cmd <- paste0(cmd, ", noise=",VecToRStr(input$sl_noise_noise, quoted=FALSE))
      cmd <- paste0(cmd, ", p=NA, delta=NA")
    }
    cmd <- paste0(cmd, ", method=",dQuote(n_method),")")
    cmd
  })

  # code for rankSwap()
  code_rankSwap <- reactive({
    cmd <- paste0("sdcObj <- rankSwap(obj=sdcObj")
    if (is.null(input$sel_rankswap_v)) {
      cmd <- paste0(cmd, ", variables=NULL")
    } else {
      cmd <- paste0(cmd, ", variables=",VecToRStr(input$sel_rankswap_v, quoted=TRUE))
    }
    cmd <- paste0(cmd, ", TopPercent=",input$sl_rankswap_top)
    cmd <- paste0(cmd, ", BottomPercent=",input$sl_rankswap_bot)
    cmd <- paste0(cmd, ", K0=",input$sl_rankswap_k0)
    cmd <- paste0(cmd, ", R0=",input$sl_rankswap_r0)
    cmd <- paste0(cmd, ", P=",input$sl_rankswap_p)
    cmd <- paste0(cmd, ", missing=NA, seed=NULL)")
    cmd
  })

  # code for l-diversity
  code_ldiv <- reactive({
    cmd <- paste0("sdcObj <- ldiversity(obj=sdcObj")
    cmd <- paste0(cmd, ", ldiv_index=",VecToRStr(input$ldiv_sensvar, quoted=TRUE))
    cmd <- paste0(cmd, ", l_recurs_c=",input$ldiv_recconst)
    cmd <- paste0(cmd, ", missing=-999)")
    cmd
  })

  # code for l-diversity
  code_suda2 <- reactive({
    cmd <- paste0("sdcObj <- suda2(obj=sdcObj")
    cmd <- paste0(cmd, ", DisFraction=",input$suda2_disf,")")
    cmd
  })

  # code to undo the last operation (if possible)
  # create a new randomized ID
  code_undo <- reactive({
    cmd <- paste0("sdcObj <- undolast(sdcObj)")
    attributes(cmd)$evalAsIs <- TRUE
    obj$anon_performed <- head(obj$anon_performed, -1)
    cmd
  })
  ### END CODE GENERATION EXPRESSIONS ####

  ### EVENTS ###
  # read data if fileInput() has been triggered
  observeEvent(input$file1, {
    obj$microfilename <- input$file1$name
    ptm <- proc.time()
    code <- code_readMicrodata()
    eval(parse(text=code))
    if ("simpleError" %in% class(res)) {
      obj$last_error <- res$message
      obj$inputdata <- NULL
      obj$code_read <- NULL
      return(NULL)
      # an error has occured -> warn the user!
    } else {
      if (!"data.frame" %in% class(res)) {
        obj$last_error <- "data read into the system was not of class 'data.frame'"
        obj$inputdata <- NULL
        obj$code_read <- NULL
        return(NULL)
      }
      obj$last_error <- NULL
      obj$inputdata <- res
      code_out <- gsub(input$file1$datapath, input$file1$name, code)
      code_out <- gsub("res", "inputdata", code_out)
      obj$code_read_and_modify <- code_out
      obj$inputdataB <- obj$inputdata
      obj$code_read_and_modify <- c(obj$code_read_and_modify,"inputdataB <- inputdata")
      obj$sdcObj <- NULL # start fresh
      ptm <- proc.time()-ptm
      obj$comptime <- obj$comptime+ptm[3]
    }
  })
  # use existing data.frame from global environment
  observeEvent(input$btn_chooose_df, {
    ptm <- proc.time()
    cmd <- code_useRObj()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    obj$code_read_and_modify <- c(obj$code_read_and_modify,"inputdataB <- inputdata")
    obj$inputdataB <- obj$inputdata
    obj$sdcObj <- NULL # start fresh
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    obj$microfilename <- input$sel_choose_df
  })
  # reset variables in inputdataset to their original values
  observeEvent(input$btn_resetmicrovar, {
    ptm <- proc.time()
    cmd <- code_resetmicrovar()
    evalcmd <- gsub("inputdata","obj$inputdata", cmd)
    eval(parse(text=evalcmd))
    obj$code_read_and_modify <- c(obj$code_read_and_modify,cmd)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    updateSelectInput(session, "sel_moddata",selected="view_var")
  })
  # undo-button
  observeEvent(input$btn_undo, {
    ptm <- proc.time()
    cmd <- code_undo()
    runEvalStr(cmd=cmd, comment=NULL)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    updateSelectInput(session, "sel_anonymize", selected = "manage_sdcProb")
    updateNavbarPage(session, "mainnav", selected="Anonymize")
  })
  # export data
  observeEvent(input$b_export, {
    ptm <- proc.time()
    session$sendCustomMessage(type='testmessage', message='Export-Button was clicked!')
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
  })
  # recode to factor
  observeEvent(input$btn_recode_to_factor, {
    ptm <- proc.time()
    cmd <- code_globalRecodeMicrodata()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    # Reset Input
    updateSelectInput(session, "sel_custom_split", selected="no")
    updateSelectInput(session, "sel_algo", selected="equidistant")
    updateTextInput(session, "txt_custom_breaks", value = "")

    # Switch to overview-page
    updateSelectInput(session, inputId="sel_moddata", selected="view_var")

    # Todo: check why this doesn't work.
    #if (input$sel_custom_split=="no") {
    #  vv <- input$sel_num_glrec[1]
    #} else {
    #  vv <- input$rb_num_glrec
    #}
    #allV <- allVars()
    #updateSelectInput(session, inputId="view_selvar2", choices=c("none", allV), selected="none")
    #updateSelectInput(session, inputId="view_selvar1", choices=allV, selected=vv)
  })
  # recode to factor
  observeEvent(input$btn_recode_to_numeric, {
    ptm <- proc.time()
    cmd <- code_globalRecodeMicrodataToNum()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    # Switch to overview-page
    updateSelectInput(session, inputId="sel_moddata", selected="view_var")
  })
  # setup the sdcMicroObj
  observeEvent(input$btn_setup_sdc, {
    cmd <- code_createSdcObj()
    toF <- cmd$recode_to_factors
    if (!is.null(toF)) {
      for (i in 1:length(toF)) {
        ptm <- proc.time()
        runEvalStrMicrodat(cmd=toF[[i]], comment=NULL)
        ptm <- proc.time()-ptm
        obj$comptime <- obj$comptime+ptm[3]
      }
    }

    newS <- cmd$create_strata
    if (!is.null(newS)) {
      ptm <- proc.time()
      runEvalStrMicrodat(cmd=newS, comment="## create stratification variable")
      ptm <- proc.time()-ptm
      obj$comptime <- obj$comptime+ptm[3]
    }
    ptm <- proc.time()
    cmd <- cmd$setup_sdc
    eval(parse(text=cmd))
    cmd <- gsub("obj[$]sdcObj","sdcObj", cmd)
    cmd <- gsub("obj[$]inputdata","inputdata",cmd)
    obj$code_setup <- cmd
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    # create slider-outputs for k-Anon by group
    # this is required because otherwise these sliders would always be updating
    nrKeyVars <- isolate(length(get_keyVars()))
    obj$sls <- lapply(1:nrKeyVars, function(i) {
      id <- paste0("sl_kanon_combs_", i)
      sliderInput(id, label=h5(paste("k-Anonymity-parameter for", i, "combs")),
        value=input[[id]], width="100%", min=2, max=50, step=1)
    })
    obj$rbs <- lapply(1:nrKeyVars, function(i) {
      id <- paste0("rb_kanon_usecombs_", i)
      radioButtons(id, label=h5(paste("Apply k-Anon to all subsets of",i,"key variables?")),
        selected=input[[id]], width="100%", inline=TRUE, choices=c("Yes", "No"))
    })
    updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })

  # add ghost-vars to an existing sdcMicroObj
  observeEvent(input$btn_addGhostVars, {
    ptm <- proc.time()
    cmd <- code_addGhostvars()
    runEvalStr(cmd=cmd, comment="## Adding linked (ghost)-Variables")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    if (is.null(lastError())) {
      obj$lastaction <- "Added Ghost-variables"
      obj$anon_performed <- c(obj$anon_performed, "Added Ghost-variables")
    }
    updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })
  # add new random IDs to an existing sdcMicroObj
  observeEvent(input$btn_addRandID, {
    ptm <- proc.time()
    cmd <- code_addRandID()
    runEvalStr(cmd=cmd, comment="## Adding a new randomized ID-variable")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    if (is.null(lastError())) {
      obj$lastaction <- paste("Create new ID-variable:", input$txt_randid_newid)
      obj$anon_performed <- c(obj$anon_performed, "Create a new random ID-variable")
    }
    updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })
  # reset the sdcMicroObj
  observeEvent(input$btn_reset_sdc1, {
    obj$reset_sdc1 <- 1
  })
  observeEvent(input$btn_reset_sdc, {
    ptm <- proc.time()
    obj$sdcObj <- NULL
    obj$code_anonymize <- c()
    obj$code_setup <- c()
    obj$anon_performed <- NULL
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    obj$reset_sdc1 <- 0
    updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })

  # reset the inputdata by setting obj$inputdata to NULL
  observeEvent(input$btn_reset_inputdata1, {
    obj$reset_inputdata1 <- 1
  })
  observeEvent(input$btn_reset_inputdata, {
    #cat(paste("'btn_reset_inputdata' was clicked",input$btn_reset_inputdata,"times..!\n"))
    ptm <- proc.time()
    obj$inputdata <- NULL
    obj$code_read_and_modify <- c()
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    obj$reset_inputdata1 <- 0
  })
  # reset errors while reading data
  observeEvent(input$btn_reset_inputerror, {
    ptm <- proc.time()
    obj$last_error <- NULL
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
  })
  observeEvent(input$btn_reset_inputerror2, {
    ptm <- proc.time()
    obj$last_error <- NULL
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
  })
  # event to generate stratification variable!
  observeEvent(input$btn_create_stratavar, {
    ptm <- proc.time()
    cmd <- code_generateStrata()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    updateSelectInput(session, "sel_moddata", selected = "show_microdata")
    updateNavbarPage(session, "mainnav", selected="Microdata")
  })
  # event to update/modify an existing factor variable
  observeEvent(input$btn_update_factor, {
    ptm <- proc.time()
    cmd <- code_groupAndRename()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    #updateSelectInput(session, "sel_moddata", selected = "show_microdata")
    #updateNavbarPage(session, "mainnav", selected="Microdata")
  })
  # event to use only a subset of the available microdata
  observeEvent(input$btn_sample_microdata, {
    ptm <- proc.time()
    cmd <- code_subsetMicrodata()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    updateSelectInput(session, "sel_moddata", selected = "show_microdata")
    updateNavbarPage(session, "mainnav", selected="Microdata")
  })
  # event to set values to na in inputdata
  observeEvent(input$btn_set_to_na, {
    ptm <- proc.time()
    cmd <- code_set_to_na()
    runEvalStrMicrodat_no_errorchecking(cmd=cmd, comment="## Set specific values to 'NA'")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    updateSelectInput(session, "sel_moddata", selected = "show_microdata")
    updateNavbarPage(session, "mainnav", selected="Microdata")
  })
  ### anonymization methods (categorical) ###
  # pram() with given transition-matrix
  observeEvent(input$btn_pram_expert, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="performing pram() (this might take a long time)...", value = 0)
    ptm <- proc.time()
    cmd <- code_pram_expert()
    runEvalStr(cmd=cmd, comment="## Postrandomization (using a transition matrix)")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    progress$set(message="performing pram() (this might take a long time)...", value = 1)
    if (is.null(lastError())) {
      obj$lastaction <- paste("Postrandomization of variable(s):", paste(input$sel_pramvars_expert, collapse=', '))
      obj$anon_performed <- c(obj$anon_performed, "Postrandomization of categorical variables (expert use)")
    }
    #updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    #updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })
  # pram() with parameters 'pd' and 'alpha'
  observeEvent(input$btn_pram_nonexpert, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="performing pram() (this might take a long time)...", value = 0)
    ptm <- proc.time()
    cmd <- code_pram_nonexpert()
    runEvalStr(cmd=cmd, comment="## Postrandomization (using a invariant, randomly generated transition matrix)")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    progress$set(message="performing pram() (this might take a long time)...", value = 1)

    if (is.null(lastError())) {
      obj$lastaction <- paste("Postrandomization of variable(s):", paste(input$sel_pramvars_nonexpert, collapse=', '))
      obj$anon_performed <- c(obj$anon_performed, "Postrandomization of categorical variables")
    }
    #updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    #updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })
  # kAnon()
  observeEvent(input$btn_kanon, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="performing kAnon() (this might take a long time)...", value = 0)
    ptm <- proc.time()
    cmd <- code_kAnon()
    runEvalStr(cmd=cmd, comment="## kAnonymity")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    progress$set(message="performing kAnon() (this might take a long time)...", value = 1)
    if (is.null(lastError())) {
      obj$lastaction <- "Establishing k-Anonymity"
      obj$anon_performed <- c(obj$anon_performed, "Establishing k-anonymity")
    }
  })
  # suppress risky observations
  observeEvent(input$btn_supp_th, {
    ptm <- proc.time()
    cmd <- code_suppThreshold()
    runEvalStr(cmd=cmd, comment="## Suppression of risky observations")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    if (is.null(lastError())) {
      obj$lastaction <- paste("Supress risky records by threshold in variable", dQuote(input$sel_supp_th_var))
      obj$anon_performed <- c(obj$anon_performed, "Suppressing high-risk values")
    }
    updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })
  # event to update/modify an existing factor variable
  observeEvent(input$btn_update_recfac, {
    ptm <- proc.time()
    cmd <- code_groupAndRename_keyvar()
    runEvalStr(cmd=cmd, comment=NULL)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    if (is.null(lastError())) {
      obj$lastaction <- paste("Recoding of key-variable:", dQuote(input$sel_recfac),"\n")
      obj$anon_performed <- c(obj$anon_performed, "Recoding of categorical key-variables")
    }
  })

  ### anonymization methods (numerical) ###
  observeEvent(input$btn_topbotcoding_num, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="performing topBotCoding() (this might take a long time)...", value = 0)
    ptm <- proc.time()
    cmd <- code_topBotCoding_num()
    runEvalStr(cmd=cmd, comment="## Performing top/bottom-coding")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    if (is.null(lastError())) {
      obj$lastaction <- paste("Performing top-bottom coding to variable(s)", shQuote(input$sel_topbot_var_num))
      obj$anon_performed <- c(obj$anon_performed, "Top-/Bottom coding of numeric variables")
    }
    progress$set(message="performing topBotCoding() (this might take a long time)...", value = 1)
    updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })

  # microaggregation()
  observeEvent(input$btn_microagg, {
    # Create a Progress object and close it on exit
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="performing Microaggregation (this might take a long time)...", value = 0)
    ptm <- proc.time()
    complete_cmd <- code_microaggregation()
    cmd <- complete_cmd$cmd
    cmd_reset1 <- complete_cmd$cmd_reset_strata$cmd1
    cmd_reset2 <- complete_cmd$cmd_reset_strata$cmd2

    # temporarily reset strata-variable
    if (!is.na(cmd_reset1)) {
      attributes(cmd_reset1)$evalAsIs <- TRUE
      runEvalStr(cmd=cmd_reset1, comment="## Temporarily reset Stratification variable")
    }
    if (is.null(lastError())) {
      runEvalStr(cmd=cmd, comment="## Microaggregation")
      # reset strata-variable back to original
      if (!is.na(cmd_reset2)) {
        attributes(cmd_reset2)$evalAsIs <- TRUE
        runEvalStr(cmd=cmd_reset2, comment="## Reset Stratification variable")
      }
    }
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    progress$set(message="performing Microaggregation (this might take a long time)...", value = 1)
    if (is.null(lastError())) {
      if (is.null(input$sel_microagg_v)) {
        obj$lastaction <- paste("Microaggregation of variable(s)", paste(get_numVars_names(), collapse=', '))
      } else {
        obj$lastaction <- paste("Microaggregation of variable(s)", paste(input$sel_microagg_v, collapse=', '))
      }
      obj$anon_performed <- c(obj$anon_performed, "Microaggregation of numeric variables")
    }
    updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })
  # addNoise()
  observeEvent(input$btn_noise, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="performing addNoise() (this might take a long time)...", value = 0)
    ptm <- proc.time()
    cmd <- code_addNoise()
    runEvalStr(cmd=cmd, comment="## Adding stochastic noise")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    if (is.null(lastError())) {
      if (is.null(input$sel_noise_v)) {
        obj$lastaction <- paste("Adding noise to variable(s)", paste(get_numVars_names(), collapse=', '))
      } else {
        obj$lastaction <- paste("Adding noise to variable(s)", paste(input$sel_noise_v, collapse=', '))
      }
      obj$anon_performed <- c(obj$anon_performed, "Adding stochastic noise to numeric variables")
    }
    progress$set(message="performing addNoise() (this might take a long time)...", value = 1)
    updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })
  # rankSwap()
  observeEvent(input$btn_rankswap, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="performing rankSwap() (this might take a long time)...", value = 0)
    ptm <- proc.time()
    cmd <- code_rankSwap()
    runEvalStr(cmd=cmd, comment="## Performing rankSwapping")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    if (is.null(lastError())) {
      if (is.null(input$sel_rankswap_v)) {
        obj$lastaction <- paste("Performing rank-swapping to variable(s)", paste(get_numVars_names(), collapse=', '))
      } else {
        obj$lastaction <- paste("Performing rank-swapping to variable(s)", paste(input$sel_rankswap_v, collapse=', '))
      }
      obj$anon_performed <- c(obj$anon_performed, "Rank-swapping of numeric variables")
    }
    progress$set(message="performing rankSwap() (this might take a long time)...", value = 1)
    updateRadioButtons(session, "sel_anonymize",choices=choices_anonymize(), selected="manage_sdcProb")
    updateRadioButtons(session, "sel_sdcresults",choices=choices_anon_manage(), selected="sdcObj_summary")
  })

  ### risk-measurements ###
  # ldiversity()
  observeEvent(input$btn_ldiv, {
    ptm <- proc.time()
    cmd <- code_ldiv()
    runEvalStr(cmd=cmd, comment="## calculating l-diversity measure")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
  })
  # suda2()
  observeEvent(input$btn_suda2, {
    ptm <- proc.time()
    cmd <- code_suda2()
    runEvalStr(cmd=cmd, comment="## calculating suda2 risk-measure")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
  })

  ## reproducibility ##
  #observeEvent(input$btn_exportProblem, {
  #cat(paste("'btn_exportProblem' was clicked",input$btn_exportProblem,"times..!\n"))
  #  ptm <- proc.time()
  #  cmd <- code_undo()
  #  runEvalStr(cmd=cmd, comment=NULL)
  #  ptm <- proc.time()-ptm
  #  obj$comptime <- obj$comptime+ptm[3]
  #  updateSelectInput(session, "sel_anonymize", selected = "manage_sdcProb")
  #  updateNavbarPage(session, "mainnav", selected="Anonymize")
  #})

  observeEvent(input$file_importProblem, {
    ptm <- proc.time()
    code <- code_import_problem()
    eval(parse(text=code))
    if ("simpleError" %in% class(res)) {
      obj$last_error <- res$message
      updateNavbarPage(session, "mainnav", selected="Reproducibility")
      #return(NULL)
      # an error has occured -> warn the user!
    } else {
      if (!"sdcMicro_GUI_export" %in% class(res)) {
        obj$last_error <- "data read into the system was not of class 'sdcMicro_GUI_export'"
        updateNavbarPage(session, "mainnav", selected="Reproducibility")
        #return(NULL)
      } else {
        obj$last_error <- NULL
        obj$inputdata <- res$inputdata
        obj$last_warning <- res$last_warning
        obj$code <- res$code
        obj$sdcObj <- res$sdcObj
        obj$code_anonymize <- res$code_anonymize
        obj$transmat <- res$transmat
        obj$inputdataB <- res$inputdataB
        obj$code_read_and_modify <- res$code_read_and_modify
        rm(res)
        ptm <- proc.time()-ptm
        obj$comptime <- obj$comptime+ptm[3]
        updateNavbarPage(session, "mainnav", selected="Anonymize")
      }
    }
  })

  # create links to sdcProblem
  lapply(href_to_setup, function(x) {
    eval(parse(text=x))
  })
  # create links to microdata-upload page
  lapply(href_to_microdata, function(x) {
    eval(parse(text=x))
  })
})
