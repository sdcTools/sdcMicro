library(shiny)

shinyServer(function(session, input, output) {
  for (file in list.files("controllers")) {
    source(file.path("controllers", file), local=TRUE)
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
    cmd <- paste0("inputdata[[",dQuote(input$view_selvar),"]] <- inputdataB[[",dQuote(input$view_selvar),"]]")
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
      cmd <- paste0(cmd, ", var=",dQuote(input$sel_num),")")
    } else {
      cmd <- paste0("inputdata <- globalRecode(obj=inputdata")
      cmd <- paste0(cmd, ", column=",dQuote(input$sel_num))
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

  # set values to na in inputdata
  code_set_to_na <- reactive({
    cmd <- paste0("inputdata[",input$num_na_suppid,",",VecToRStr(input$sel_na_suppvar),"] <- NA")
    cmd
  })

  # code for top/bottom coding
  code_topBotCoding <- reactive({
    cmd <- paste0("inputdata <- topBotCoding(inputdata, column=",dQuote(input$sel_topbot_var))
    cmd <- paste0(cmd, ", value=",as.numeric(input$num_topbot_val))
    cmd <- paste0(cmd, ", replacement=",as.numeric(input$num_topbot_replacement))
    cmd <- paste0(cmd, ", kind=",dQuote(input$sel_topbot_kind),")")
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
    cmd <- paste0(cmd,"sdcObj <- pram(sdcObj, variables=",dQuote(input$sel_pramvars_expert),", pd=mat)")
    cmd
  })

  # code for non-expert pram-application
  code_pram_nonexpert <- reactive({
    cmd <- paste0("sdcObj <- pram(sdcObj, variables=",VecToRStr(input$sel_pramvars_nonexpert,quoted=TRUE))
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
      k <- input$sl_kanon_k
    }
    cmd <- paste0(cmd, ", combs=NULL, k=",VecToRStr(k, quoted=FALSE),")")
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
    wV <- hhId <- strataVar <- NULL
    if (length(input$sel_nV)==1 && input$sel_nV=="") {
      nV <- NULL
    } else {
      nV <- input$sel_nV
    }
    if (input$sel_wV!="none") {
      wV <- input$sel_wV
    }
    if (input$sel_hhID!="none") {
      hhId <- input$sel_hhID
    }
    if (input$sel_strataV!="none") {
      strataVar <- input$sel_strataV
    }
    if (!is.null(input$sel_removeVars)) {
      excludeVars <- input$sel_removeVars
    } else {
      excludeVars <- NULL
    }
    cmd <- paste0("obj$sdcObj <- createSdcObj(dat=obj$inputdata,")
    cmd <- paste0(cmd, "\n\tkeyVars=",VecToRStr(input$sel_kV, quoted=TRUE))
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
    if (!is.null(excludeVars)) {
      cmd <- paste0(cmd, ", \n\texcludeVars=",VecToRStr(excludeVars, quoted=TRUE))
    } else {
      cmd <- paste0(cmd, ", \n\texcludeVars=NULL")
    }
    cmd <- paste0(cmd, ", \n\tseed=",input$sl_ranseed)

    if (input$rb_setup_randomizeorder) {
      cmd <- paste0(cmd, ", \n\trandomizeRecords=TRUE")
    } else {
      cmd <- paste0(cmd, ", \n\trandomizeRecords=FALSE")
    }
    cmd <- paste0(cmd,")")
    cmd
  })

  # code to add ghost-vars to existing key variables
  code_addGhostVars <- reactive({
    cmd <- paste0("sdcObj <- addGhostVars(obj=sdcObj, keyVar=",dQuote(input$sel_gv1),", ghostVars=c(",dQuote(paste(input$sel_gv2, collapse='","')), "))")
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
    cmd
  })
  ### END CODE GENERATION EXPRESSIONS ####

  ### EVENTS ###
  # read data if fileInput() has been triggered
  observeEvent(input$file1, {
    #cat("input$file1 (",input$file1$name,") has been pressed!\n")
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
    }
  })
  # use existing data.frame from global environment
  observeEvent(input$btn_chooose_df, {
    #cat(paste("'btn_chooose_df' was clicked",input$btn_chooose_df,"times..!\n"))
    cmd <- code_useRObj()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    #obj$code_read_and_modify <- tail(obj$code_read_and_modify, 1)
    #obj$code_read_and_modify <- c(obj$code_read_and_modify, cmd)
    obj$code_read_and_modify <- c(obj$code_read_and_modify,"inputdataB <- inputdata")
    obj$inputdataB <- obj$inputdata
    obj$sdcObj <- NULL # start fresh
  })
  # reset variables in inputdataset to their original values
  observeEvent(input$btn_resetmicrovar, {
    #cat(paste("'btn_resetmicrovar' was clicked",input$btn_resetmicrovar,"times..!\n"))
    cmd <- code_resetmicrovar()
    evalcmd <- gsub("inputdata","obj$inputdata", cmd)
    eval(parse(text=evalcmd))
    obj$code_read_and_modify <- c(obj$code_read_and_modify,cmd)
    updateSelectInput(session, "sel_moddata",selected="View/Analyse a variable")
  })
  # undo-button
  observeEvent(input$btn_undo, {
    #cat(paste("'btn_undo' was clicked",input$btn_undo,"times..!\n"))
    cmd <- code_undo()
    runEvalStr(cmd=cmd, comment=NULL)
    updateSelectInput(session, "sel_anonymize", selected = "manage_sdcProb")
    updateNavbarPage(session, "mainnav", selected="Anonymize")
  })
  # export data
  observeEvent(input$b_export, {
    #cat(paste("'b_export' was clicked",input$b_export,"times..!\n"))
    session$sendCustomMessage(type='testmessage', message='Export-Button was clicked!')
  })
  # recode to factor
  observeEvent(input$btn_recode_to_factor, {
    #cat(paste("'btn_recode_to_factor' was clicked",input$btn_recode_to_factor,"times..!\n"))
    cmd <- code_globalRecodeMicrodata()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    obj$code_read_and_modify <- c(obj$code_read_and_modify, cmd)
    updateSelectInput(session, "sel_moddata",selected="View/Analyse a variable")
  })
  # setup the sdcMicroObj
  observeEvent(input$btn_setup_sdc, {
    #cat(paste("'btn_setup_sdc' was clicked",input$btn_setup_sdc,"times..!\n"))
    cmd <- code_createSdcObj()
    eval(parse(text=cmd))
    cmd <- gsub("obj[$]sdcObj","sdcObj", cmd)
    cmd <- gsub("obj[$]inputdata","inputdata",cmd)
    obj$code_read_and_modify <- c(obj$code_read_and_modify, cmd)
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })
  # add ghost-vars to an existing sdcMicroObj
  observeEvent(input$btn_addGhostVars, {
    #cat(paste("'btn_addGhostVars' was clicked",input$btn_addGhostVars,"times..!\n"))
    cmd <- code_addGhostvars()
    runEvalStr(cmd=cmd, comment="## Adding linked (ghost)-Variables")
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })
  # add new random IDs to an existing sdcMicroObj
  observeEvent(input$btn_addRandID, {
    #cat(paste("'btn_addRandID' was clicked",input$btn_addRandID,"times..!\n"))
    cmd <- code_addRandID()
    runEvalStr(cmd=cmd, comment="## Adding a new randomized ID-variable")
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })
  # reset the sdcMicroObj
  observeEvent(input$btn_reset_sdc, {
    #cat(paste("'btn_reset_sdc' was clicked",input$btn_reset_sdc,"times..!\n"))
    obj$sdcObj <- NULL
    obj$code_anonymize <- c()
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })
  # reset the inputdata by setting obj$inputdata to NULL
  observeEvent(input$btn_reset_inputdata, {
    #cat(paste("'btn_reset_inputdata' was clicked",input$btn_reset_inputdata,"times..!\n"))
    obj$inputdata <- NULL
    obj$code_read_and_modify <- c()
  })
  # reset errors while reading data
  observeEvent(input$btn_reset_inputerror, {
    #cat(paste("'btn_reset_inputerror' was clicked",input$btn_reset_inputerror,"times..!\n"))
    obj$last_error <- NULL
  })
  observeEvent(input$btn_reset_inputerror2, {
    #cat(paste("'btn_reset_inputerror2' was clicked",input$btn_reset_inputerror,"times..!\n"))
    obj$last_error <- NULL
  })
  # event to generate stratification variable!
  observeEvent(input$btn_create_stratavar, {
    #cat(paste("'btn_create_stratavar' was clicked",input$btn_create_stratavar,"times..!\n"))
    cmd <- code_generateStrata()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    updateSelectInput(session, "sel_moddata", selected = "show_microdata")
    updateNavbarPage(session, "mainnav", selected="Microdata")
  })
  # event to update/modify an existing factor variable
  observeEvent(input$btn_update_factor, {
    #cat(paste("'btn_update_factor' was clicked",input$btn_update_factor,"times..!\n"))
    cmd <- code_groupAndRename()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    updateSelectInput(session, "sel_moddata", selected = "show_microdata")
    updateNavbarPage(session, "mainnav", selected="Microdata")
  })
  # event to use only a subset of the available microdata
  observeEvent(input$btn_sample_microdata, {
    cat(paste("'btn_sample_microdata' was clicked",input$btn_sample_microdata,"times..!\n"))
    cmd <- code_subsetMicrodata()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    updateSelectInput(session, "sel_moddata", selected = "show_microdata")
    updateNavbarPage(session, "mainnav", selected="Microdata")
  })
  # event to set values to na in inputdata
  observeEvent(input$btn_set_to_na, {
    #cat(paste("'btn_set_to_na' was clicked",input$btn_set_to_na,"times..!\n"))
    cmd <- code_set_to_na()
    runEvalStrMicrodat_no_errorchecking(cmd=cmd, comment="## Set specific values to 'NA'")
    updateSelectInput(session, "sel_moddata", selected = "show_microdata")
    updateNavbarPage(session, "mainnav", selected="Microdata")
  })
  # event to apply top-/bottomcoding
  observeEvent(input$btn_topbotcoding, {
    #cat(paste("'btn_topbotcoding' was clicked",input$btn_topbotcoding,"times..!\n"))
    cmd <- code_topBotCoding()
    runEvalStrMicrodat(cmd=cmd, comment="## Apply Top-/Bottom coding")
    updateSelectInput(session, "sel_moddata", selected = "show_microdata")
    updateNavbarPage(session, "mainnav", selected="Microdata")
  })
  ### anonymization methods (categorical) ###
  # pram() with given transition-matrix
  observeEvent(input$btn_pram_expert, {
    #cat(paste("'btn_pram_expert' was clicked",input$btn_pram_expert,"times..!\n"))
    cmd <- code_pram_expert()
    runEvalStr(cmd=cmd, comment="## Postrandomization (using a transition matrix)")
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })
  # pram() with parameters 'pd' and 'alpha'
  observeEvent(input$btn_pram_nonexpert, {
    #cat(paste("'btn_pram_nonexpert' was clicked",input$btn_pram_nonexpert,"times..!\n"))
    cmd <- code_pram_nonexpert()
    runEvalStr(cmd=cmd, comment="## Postrandomization (using a invariant, randomly generated transition matrix)")
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })
  # kAnon()
  observeEvent(input$btn_kanon, {
    #cat(paste("'btn_kanon' was clicked",input$btn_kanon,"times..!\n"))
    cmd <- code_kAnon()
    runEvalStr(cmd=cmd, comment="## kAnonymity")
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })
  # suppress risky observations
  observeEvent(input$btn_supp_th, {
    #cat(paste("'btn_supp_th' was clicked",input$btn_supp_th,"times..!\n"))
    cmd <- code_suppThreshold()
    runEvalStr(cmd=cmd, comment="## Suppression of risky observations")
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })
  # event to update/modify an existing factor variable
  observeEvent(input$btn_update_recfac, {
    cmd <- code_groupAndRename_keyvar()
    runEvalStr(cmd=cmd, comment=NULL)
  })

  ### anonymization methods (numerical) ###
  # microaggregation()
  observeEvent(input$btn_microagg, {
    #cat(paste("'btn_microagg' was clicked",input$btn_microagg,"times..!\n"))
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
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })
  # addNoise()
  observeEvent(input$btn_noise, {
    #cat(paste("'btn_noise' was clicked",input$btn_noise,"times..!\n"))
    cmd <- code_addNoise()
    runEvalStr(cmd=cmd, comment="## Adding stochastic noise")
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })
  # rankSwap()
  observeEvent(input$btn_rankswap, {
    #cat(paste("'btn_rankswap' was clicked",input$btn_rankswap,"times..!\n"))
    cmd <- code_rankSwap()
    runEvalStr(cmd=cmd, comment="## Performing rankSwapping")
    updateSelectInput(session, "sel_anonymize",selected="View/Analyse existing sdcProblem")
  })

  ### risk-measurements ###
  # ldiversity()
  observeEvent(input$btn_ldiv, {
    #cat(paste("'btn_ldiv' was clicked",input$btn_ldiv,"times..!\n"))
    cmd <- code_ldiv()
    runEvalStr(cmd=cmd, comment="## calculating l-diversity measure")
  })
  # suda2()
  observeEvent(input$btn_suda2, {
    #cat(paste("'btn_suda2' was clicked",input$btn_suda2,"times..!\n"))
    cmd <- code_suda2()
    runEvalStr(cmd=cmd, comment="## calculating suda2 risk-measure")
  })

  ## reproducibility ##
  #observeEvent(input$btn_exportProblem, {
  #  #cat(paste("'btn_undo' was clicked",input$btn_undo,"times..!\n"))
  #  cmd <- code_undo()
  #  runEvalStr(cmd=cmd, comment=NULL)
  #  updateSelectInput(session, "sel_anonymize", selected = "manage_sdcProb")
  #  updateNavbarPage(session, "mainnav", selected="Anonymize")
  #})

  observeEvent(input$file_importProblem, {
    #cat("input$file_importProblem (",input$file_importProblem$name,") has been pressed!\n")
    code <- code_import_problem()
    eval(parse(text=code))
    if ("simpleError" %in% class(res)) {
      obj$last_error <- res$message
      return(NULL)
      # an error has occured -> warn the user!
    } else {
      if (!"sdcMicro_GUI_export" %in% class(res)) {
        obj$last_error <- "data read into the system was not of class 'sdcMicro_GUI_export'"
        obj$inputdata <- NULL
        obj$code_read <- NULL
        return(NULL)
      }
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
      updateSelectInput(session, "sel_anonymize", selected = "manage_sdcProb")
      updateNavbarPage(session, "mainnav", selected="Anonymize")
    }
  })

  # create links to sdcProblem
  lapply(href_to_setup, function(x) {
    eval(parse(text=x))
  })
})
