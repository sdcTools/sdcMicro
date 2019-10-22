library(shiny)

shinyServer(function(session, input, output) {
  wd <- setwd(getShinyOption(".appDir", getwd()))
  on.exit(setwd(wd))

  for (file in list.files("controllers")) {
    source(file.path("controllers", file), local=TRUE)
  }
  values <- reactiveValues(starting = TRUE)
  session$onFlushed(function() {
    values$starting <- FALSE
  })

  # dynamically generate inputs (currently used in setup-sdcProblem)
  shinyInput <- function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }

  # obtain the values of inputs
  shinyValue <- function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }

  # update pram matrix
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

  # reactive expression, we use this to change the output of output$sdcObj_exists in controllers/ui_setup_sdc.R
  lastWarning <- reactive({
    return(obj$last_warning$warnMsg)
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
    obj <- input$sel_choose_df
    cmd_chk <- paste0("if (!exists(", dQuote(obj),")) {\n")
    cmd_chk <- paste0(cmd_chk, "  stop('object ", dQuote(obj), " is missing; make sure it exists.`', call. = FALSE)\n")
    cmd_chk <- paste0(cmd_chk, "}\n")

    cmd <- paste0(cmd_chk, "obj$inputdata <- readMicrodata(")
    cmd <- paste0(cmd, "path=",dQuote(obj))
    cmd <- paste0(cmd, ", type=",dQuote("rdf"))
    cmd <- paste0(cmd, ", convertCharToFac=FALSE")
    cmd <- paste0(cmd, ", drop_all_missings=FALSE)")
    cmd
  })

  # read microdata
  code_readMicrodata <- reactive({
    file1 <- fixUploadedFilesNames(input$file1)

    val <- obj$cur_selection_import
    if (val=="btn_import_data_1") {
      dattype <- "rdf"
    }
    if (val=="btn_import_data_2") {
      dattype <- "R"
    }
    if (val=="btn_import_data_3") {
      dattype <- "spss"
    }
    if (val=="btn_import_data_4") {
      dattype <- "sas"
    }
    if (val=="btn_import_data_5") {
      dattype <- "csv"
    }
    if (val=="btn_import_data_6") {
      dattype <- "stata"
    }

    cmd <- paste0("res <- readMicrodata(")
    cmd <- paste0(cmd, "path=",dQuote(normalizePath(file1$datapath,winslash = "/")))
    cmd <- paste0(cmd, ", type=",dQuote(dattype))
    cmd <- paste0(cmd, ", convertCharToFac=",input$rb_convert_c_to_f)
    cmd <- paste0(cmd, ", drop_all_missings=",input$rb_drop_all_missings)
    if (dattype=="csv") {
      cmd <- paste0(cmd, ", header=",input$import_csv_header)
      cmd <- paste0(cmd, ", sep=",dQuote(input$import_csv_sep))
    }
    cmd <- paste0(cmd,")")
    cmd
  })

  # import household-file
  code_importHHFile <- reactive({
    file_hhfile <- fixUploadedFilesNames(input$file_hhfile)
    cmd <- paste0("res <- readMicrodata(")
    cmd <- paste0(cmd, "path=",dQuote(normalizePath(file_hhfile$datapath, winslash = "/")))
    cmd <- paste0(cmd, ", type=",dQuote("R"))
    cmd <- paste0(cmd, ", convertCharToFac=",FALSE)
    cmd <- paste0(cmd, ", drop_all_missings=",FALSE)
    cmd <- paste0(cmd,")")
    cmd
  })

  # merge household-file
  code_merge_hhdata  <- reactive({
    cmd <- paste0("inputdata <- mergeHouseholdData(dat=obj$inputdata, hhId=",dQuote(input$sel_hhid_hhdata),", dathh=obj$hhdata)")
    cmd
  })

  # prepare a household-level input file
  code_prepare_hhdata <- reactive({
    cn <- colnames(inputdata())
    mm <- cn[sort(match(input$sel_hhvars, cn))]
    cmd <- paste0("inputdata <- selectHouseholdData(dat=obj$inputdata, hhId=",dQuote(input$sel_hhvars_id),", hhVars=",VecToRStr(mm, quoted=TRUE),")")
    cmd
  })

  # code to import previously saved sdcProblem
  code_import_problem <- reactive({
    file_importProblem <- fixUploadedFilesNames(input$file_importProblem)
    cmd <- paste0("res <- importProblem(path=",dQuote(normalizePath(file_importProblem$datapath,winslash = "/")),")")
    cmd
  })
  # code to import previously saved sdcProblem
  code_import_problem1 <- reactive({
    file_importProblem <- fixUploadedFilesNames(input$file_importProblem1)
    cmd <- paste0("res <- importProblem(path=",dQuote(normalizePath(file_importProblem$datapath,winslash = "/")),")")
    cmd
  })
  # code to reset variables in the input data set
  code_resetmicrovar <- reactive({
    cmd <- paste0("inputdata[, ",VecToRStr(input$sel_reset_microvars, quoted=TRUE),"] <- inputdataB[,",VecToRStr(input$sel_reset_microvars, quoted=TRUE),"]")
    cmd
  })

  # code to group or rename factor-levels
  code_groupAndRename <- reactive({
    vaddna <- FALSE
    if (input$rb_micro_addna=="yes") {
      vaddna <- TRUE
    }
    cmd <- paste0("inputdata <- groupAndRename(obj=inputdata")
    cmd <- paste0(cmd, ", var=",dQuote(input$sel_factor))
    cmd <- paste0(cmd, ", before=",VecToRStr(input$cbg_factor, quoted=TRUE))
    cmd <- paste0(cmd, ", after=",VecToRStr(input$inp_newlevname, quoted=TRUE))
    cmd <- paste0(cmd, ", addNA=",vaddna,")")
    cmd
  })

  # code to use only parts of the original microdata
  code_subsetMicrodata <- reactive({
    cmd <- paste0("inputdata <- sdcMicro:::subsetMicrodata(obj=inputdata, type=",dQuote(input$sel_sdcP_sample_type))
    cmd <- paste0(cmd, ", n=",input$sel_sdcP_sample_n,")")

    if (input$sel_sdcP_sample_type=="n_perc") {
      comment <- paste0("## Reduce dataset size - Select n percent of the original dataset")
    }
    if (input$sel_sdcP_sample_type=="first_n") {
      comment <- paste0("## Reduce dataset size - Select first n observations of the original dataset")
    }
    if (input$sel_sdcP_sample_type=="every_n") {
      comment <- paste0("## Reduce dataset size - Select every n-th observations of the original dataset")
    }
    if (input$sel_sdcP_sample_type=="size_n") {
      comment <- paste0("## Reduce dataset size - Select n randomly drawn observations from the original dataset")
    }
    return(list(cmd=cmd, comment=comment))
  })

  # creating factors from numeric variables for input data
  code_globalRecodeMicrodata <- reactive({
    if (input$sel_custom_split=="no") {
      cmd <- paste0("inputdata <- varToFactor(obj=inputdata")
      cmd <- paste0(cmd, ", var=",VecToRStr(input$sel_num_glrec, quoted=TRUE),")")
      comment <- "## Convert a numeric variable to factor (each distinct value becomes a factor level)"
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
      comment <- "## Convert a numeric variable to factor with user-specified breaks"
    }
    return(list(cmd=cmd, comment=comment))
  })
  # creating numeric variables from factors or character variables
  code_globalRecodeMicrodataToNum <- reactive({
    cmd <- paste0("inputdata <- varToNumeric(obj=inputdata")
    cmd <- paste0(cmd, ", var=",VecToRStr(input$sel_to_num_var, quoted=TRUE),")")
    comment <- "## Convert a string or factor variable to numeric"
    return(list(cmd=cmd, comment=comment))
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

    txt_action <- paste0("Linked ", VecToRStr_txt(input$sel_gv2), " to ", dQuote(input$sel_gv1), " for local suppression.")
    return(list(cmd=cmd, txt_action=txt_action))
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
    if (input$pram_expert_strataV!="no stratification") {
      res <- code_reset_strata(new_strataV=input$pram_expert_strataV, ex_strataV=NULL)
      cmd_strata1 <- res$cmd1
      cmd_strata2 <- res$cmd2
    } else {
      cmd_strata1 <- cmd_strata2 <- NA
    }

    v <- as.vector(as.matrix(obj$transmat))/100
    rn <- rownames(obj$transmat)
    matstr <- VecToRStr(v, quoted=FALSE)
    cmd <- paste0("mat <- matrix(",matstr,",ncol=",ncol(obj$transmat),"); ")
    cmd <- paste0(cmd,"\nrownames(mat) <- colnames(mat) <- ", VecToRStr(rn, quoted=TRUE),";\n")
    cmd <- paste0(cmd,"sdcObj <- pram(sdcObj, variables=",dQuote(input$sel_pramvars_expert),", pd=mat)")

    txt_action <- paste0("PRAM of categorical variables ", VecToRStr_txt(input$sel_pramvars_expert), " with user-specified transition matrix (see above)\n")
    return(list(cmd=cmd, cmd_strata1=cmd_strata1, cmd_strata2=cmd_strata2, txt_action=txt_action))
  })

  # code for non-expert pram-application
  code_pram_nonexpert <- reactive({
    if (input$pram_strataV_simple!="no stratification") {
      res <- code_reset_strata(new_strataV=input$pram_strataV_simple, ex_strataV=NULL)
      cmd_strata1 <- res$cmd1
      cmd_strata2 <- res$cmd2
    } else {
      cmd_strata1 <- cmd_strata2 <- NA
    }

    cmd <- paste0("sdcObj <- pram(sdcObj, variables=",VecToRStr(input$sel_pramvars_simple, quoted=TRUE))
    cmd <- paste0(cmd,", pd=",input$pram_simple_pd)
    cmd <- paste0(cmd,", alpha=",input$pram_simple_alpha,")")

    txt_action <- paste0("PRAM of categorical variables ", VecToRStr_txt(input$sel_pramvars_simple), " with invariant transition matrix (see above)")
    txt_action <- paste0(txt_action, " (parameters pd=", input$pram_simple_pd," and alpha=", input$pram_simple_alpha,")\n")
    return(list(cmd=cmd, cmd_strata1=cmd_strata1, cmd_strata2=cmd_strata2, txt_action=txt_action))
  })

  # code for k-Anonymity
  code_kAnon <- reactive({
    if (input$kanon_strataV!="no stratification") {
      res <- code_reset_strata(new_strataV=input$kanon_strataV, ex_strataV=NULL)
      cmd_strata1 <- res$cmd1
      cmd_strata2 <- res$cmd2
    } else {
      cmd_strata1 <- cmd_strata2 <- NA
    }

    cmd <- paste0("sdcObj <- kAnon(sdcObj")
    if (kAnon_useImportance()) {
      cur_importance <- kAnon_impvec()
    } else {
      # calculate importance just as in localSuppression()
      # this is required because we want to show the order on the summary-page
      # otherwise it would be possible just to set cur_importance to NULL
      x <- as.data.table(get_manipKeyVars())
      xx <- x[,lapply(.SD, function(y) { length(table(y))}), .SDcols=get_keyVars_names()]
      cur_importance <- match(names(xx), names(sort(xx, decreasing=FALSE)))
    }
    cmd <- paste0(cmd,", importance=",VecToRStr(cur_importance, quoted=FALSE))

    if (input$rb_kanon_useCombs=="Yes") {
      params <- kAnon_comb_params()
      cmd <- paste0(cmd,", combs=",VecToRStr(params$use, quoted=FALSE))
      k <- params$k
      txt_action <- NULL
      for (i in 1:length(k)) {
        txt_action <- paste0(txt_action, "Establishing ",k[i],"-anonymity in key variables (with following order of importance: ",VecToRStr_txt(get_keyVars_names()[order(as.numeric(cur_importance))]),") for all ",params$use[i],"-combinations of key variables.\n\n")
      }
    } else {
      cmd <- paste0(cmd, ", combs=NULL")
      k <- input$sl_kanon_k
      txt_action <- paste0("Establishing ",k,"-anonymity in key variables (with following order of importance: ",VecToRStr_txt(get_keyVars_names()[order(as.numeric(cur_importance))]),")")
    }
    cmd <- paste0(cmd, ", k=",VecToRStr(k, quoted=FALSE),")")
    return(list(cmd=cmd, cmd_strata1=cmd_strata1, cmd_strata2=cmd_strata2, txt_action=txt_action))
  })

  # code to group or a key variable (factor)
  code_groupAndRename_keyvar <- reactive({
    vaddna <- FALSE
    if (input$rb_recfac_micro_addna=="yes") {
      vaddna <- TRUE
    }
    cmd <- paste0("sdcObj <- groupAndRename(obj=sdcObj")
    cmd <- paste0(cmd, ", var=",dQuote(input$sel_recfac))
    cmd <- paste0(cmd, ", before=",VecToRStr(input$cbg_recfac, quoted=TRUE))
    cmd <- paste0(cmd, ", after=",VecToRStr(input$inp_newlevname_rec, quoted=TRUE))
    cmd <- paste0(cmd, ", addNA=",vaddna,")")
    txt_action <- paste0("Recoded ",dQuote(input$sel_recfac),": ", VecToRStr_txt(input$cbg_recfac)," to ",VecToRStr_txt(input$inp_newlevname_rec))
    return(list(cmd=cmd, txt_action=txt_action))
  })

  # code for local suppression with threshold
  code_suppThreshold <- reactive({
    cmd <- paste0("sdcObj <- localSupp(sdcObj")
    cmd <- paste0(cmd,", threshold=",input$sl_supp_threshold)
    cmd <- paste0(cmd,", keyVar=",dQuote(input$sel_supp_th_var),")")
    txt_action <- paste0("Suppress values in variable ",dQuote(input$sel_supp_th_var)," with individual risk above the threshold of ", input$sl_supp_threshold, "\n")
    return(list(cmd=cmd, txt_action=txt_action))
  })

  # code to generate a stratification-variable
  code_generateStrata <- reactive({
    cmd <- paste0("inputdata <- generateStrata(df=inputdata")
    cmd <- paste0(cmd, ", stratavars=",VecToRStr(input$sel_allvars_strata, quoted=TRUE))
    cmd <- paste0(cmd,", name=",dQuote(input$inp_vname_strata),")")

    comment <- "## Generate a new strata variable based on other variables"
    return(list(cmd=cmd, comment=comment))
  })

  # code to generate an sdcMicroObj
  code_createSdcObj <- reactive({
    cmd_out <- list()
    cmd_out$recode_to_factors <- NULL
    cmd_out$create_strata <- NULL
    cmd_out$setup_sdc <- NULL
    cmd_out$update_options <- NULL
    vars <- allVars()
    nc <- length(vars)
    type <- dataTypes()
    vv <- obj$setupval_inc
    useAsKey <- shinyValue(paste0("setup_key_",vv,"_"), nc)
    useAsPram <- shinyValue(paste0("setup_pram_",vv,"_"), nc)
    useAsWeight <- shinyValue(paste0("setup_weight_",vv,"_"), nc)
    useAsClusterID <- shinyValue(paste0("setup_cluster_",vv,"_"), nc)
    deleteVariable <- shinyValue(paste0("setup_remove_",vv,"_"), nc)

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
    strataVar <- NULL
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

    if (!is.null(obj$microfilename)) {
      cmd <- paste0("opts <- get.sdcMicroObj(obj$sdcObj, type=",dQuote("options"),")\n")
      cmd <- paste0(cmd, "opts$filename <- ", dQuote(obj$microfilename),"\n")
      cmd <- paste0(cmd, "obj$sdcObj <- set.sdcMicroObj(obj$sdcObj, type=",dQuote("options"),", input=list(opts))\n")
      cmd_out$update_options <- cmd
    }
    cmd_out
  })

  # code for top-bottom coding on sdcMicroObj
  code_topBotCoding_num <- reactive({
    cmd <- paste0("sdcObj <- topBotCoding(obj=sdcObj, column=",dQuote(input$sel_topbot_var_num))
    cmd <- paste0(cmd, ", value=",as.numeric(input$num_topbot_val_num))
    cmd <- paste0(cmd, ", replacement=",as.numeric(input$num_topbot_replacement_num))
    cmd <- paste0(cmd, ", kind=",dQuote(input$sel_topbot_kind_num),")")

    txt_action <- paste0(input$sel_topbot_kind_num,"-coding of variable ",dQuote(input$sel_topbot_var_num), " ")
    txt_action <- paste0(txt_action, "at level ", as.numeric(input$num_topbot_val_num), " (replacement value=", as.numeric(input$num_topbot_replacement_num),")\n")
    return(list(cmd=cmd, txt_action=txt_action))
  })

  # code for microaggregation()
  code_microaggregation <- reactive({
    m_method <- input$sel_microagg_method
    cmd <- NULL
    if (input$sel_microagg_strata!="no stratification") {
      res <- code_reset_strata(new_strataV=input$sel_microagg_strata, ex_strataV=NULL)
      cmd_strata1 <- res$cmd1
      cmd_strata2 <- res$cmd2
    } else {
      cmd_strata1 <- cmd_strata2 <- NA
    }
    cmd <- paste0("sdcObj <- microaggregation(obj=sdcObj")
    if (is.null(input$sel_microagg_v)) {
      cmd <- paste0(cmd, ", variables=NULL")
      txt_action <- paste0("Microaggregation of numeric variable(s) ",VecToRStr_txt(get_numVars_names()))
    } else {
      cmd <- paste0(cmd, ", variables=",VecToRStr(input$sel_microagg_v, quoted=TRUE))
      txt_action <- paste0("Microaggregation of numeric variable(s) ",VecToRStr_txt(input$sel_microagg_v))
    }
    cmd <- paste0(cmd, ", aggr=",input$sl_microagg_aggr)
    cmd <- paste0(cmd, ", method=",dQuote(m_method))

    txt_action <- paste0(txt_action, " (method=",dQuote(m_method)," and size=",input$sl_microagg_aggr,")\n")

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
    return(list(cmd=cmd, cmd_strata1=cmd_strata1, cmd_strata2=cmd_strata2, txt_action=txt_action))
  })

  # code for addNoise()
  code_addNoise <- reactive({
    n_method <- input$sel_noise_method
    cmd <- paste0("sdcObj <- addNoise(obj=sdcObj")
    if (is.null(input$sel_noise_v)) {
      cmd <- paste0(cmd, ", variables=NULL")
      txt_action <- paste0("Adding stochastic noise to variable(s) ",VecToRStr_txt(get_numVars_names()))
    } else {
      cmd <- paste0(cmd, ", variables=",VecToRStr(input$sel_noise_v, quoted=TRUE))
      txt_action <- paste0("Adding stochastic noise to variable(s) ",VecToRStr_txt(input$sel_noise_v))
    }

    txt_action <- paste0(txt_action, " (method=",dQuote(n_method)," ")

    if (n_method =="correlated2") {
      cmd <- paste0(cmd, ", delta=",VecToRStr(input$sl_noise_noise, quoted=FALSE))
      cmd <- paste0(cmd, ", noise=NA, p=NA")
      txt_action <- paste0(txt_action, " and delta=",input$sl_noise_noise,")\n")

    } else if (n_method=="ROMM") {
      cmd <- paste0(cmd, ", p=",VecToRStr(input$sl_noise_noise, quoted=FALSE))
      cmd <- paste0(cmd, ", noise=NA, delta=NA")
      txt_action <- paste0(txt_action, " and p=",input$sl_noise_noise,")\n")
    } else {
      cmd <- paste0(cmd, ", noise=",VecToRStr(input$sl_noise_noise, quoted=FALSE))
      cmd <- paste0(cmd, ", p=NA, delta=NA")
      txt_action <- paste0(txt_action, " and noise=",input$sl_noise_noise,")\n")

    }
    cmd <- paste0(cmd, ", method=",dQuote(n_method),")")
    return(list(cmd=cmd, txt_action=txt_action))
  })

  # code for rankSwap()
  code_rankSwap <- reactive({
    cmd <- paste0("sdcObj <- rankSwap(obj=sdcObj")
    if (is.null(input$sel_rankswap_v)) {
      cmd <- paste0(cmd, ", variables=NULL")
      txt_action <- paste0("Rank swapping of variable(s) ",VecToRStr_txt(get_numVars_names()))
    } else {
      cmd <- paste0(cmd, ", variables=",VecToRStr(input$sel_rankswap_v, quoted=TRUE))
      txt_action <- paste0("Rank swapping of variable(s) ",VecToRStr_txt(input$sel_rankswap_v))
    }
    cmd <- paste0(cmd, ", TopPercent=",input$sl_rankswap_top)
    cmd <- paste0(cmd, ", BottomPercent=",input$sl_rankswap_bot)
    rankswap_k0 <- rankswap_r0 <- rankswap_p <- "NULL"
    if(input$rb_rankswap_p_k0_r0=="K0"){
      rankswap_k0 <- input$sl_rankswap_p_k0_r0
    }else if(input$rb_rankswap_p_k0_r0=="R0"){
      rankswap_r0 <- input$sl_rankswap_p_k0_r0
    }else if(input$rb_rankswap_p_k0_r0=="P"){
      rankswap_p <- input$sl_rankswap_p_k0_r0*100
    }
    cmd <- paste0(cmd, ", K0=",rankswap_k0)
    cmd <- paste0(cmd, ", R0=",rankswap_r0)
    cmd <- paste0(cmd, ", P=",rankswap_p)
    cmd <- paste0(cmd, ", missing=NA, seed=NULL)")

    txt_action <- paste0(txt_action, " (parameters: TopPercent=",input$sl_rankswap_top,", BottomPercent=", input$sl_rankswap_bot)
    txt_action <- paste0(txt_action, ", K0=",rankswap_k0,", R0=", rankswap_r0, ", P=",rankswap_p,")\n")
    return(list(cmd=cmd, txt_action=txt_action))
  })

  # code for l-diversity
  code_ldiv <- reactive({
    cmd <- paste0("sdcObj <- ldiversity(obj=sdcObj")
    cmd <- paste0(cmd, ", ldiv_index=",VecToRStr(input$ldiv_sensvar, quoted=TRUE))
    cmd <- paste0(cmd, ", l_recurs_c=",input$ldiv_recconst)
    cmd <- paste0(cmd, ", missing=NA)")
    cmd
  })

  # code for l-diversity
  code_suda2 <- reactive({
    cmd <- paste0("sdcObj <- suda2(obj=sdcObj")
    cmd <- paste0(cmd, ", DisFraction=",input$suda2_disf,", missing=NA)")
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

  # create an sdc report
  code_report <- reactive({
    input$myRepDownload # required for timestamp!
    internal <- ifelse(input$rb_simple_report=="internal", TRUE, FALSE)
    if (internal) {
      tmpF <- paste0("sdcReport_internal_",format(Sys.time(), "%Y%m%d_%H%M"))
    } else {
      tmpF <- paste0("sdcReport_external_",format(Sys.time(), "%Y%m%d_%H%M"))
    }
    cmd <- paste0("report(obj=sdcObj, outdir=",dQuote(obj$path_export),", filename=",dQuote(tmpF),', title="SDC-Report", internal=',internal,")")
    attributes(cmd)$evalAsIs <- TRUE
    return(list(cmd=cmd, path=obj$path_export, fout=tmpF))
  })

  # export/save the anonymized file
  code_export_anondata <- reactive({
    input$btn_export_anon_data # required for timestamp!
    if (input$dat_exp_type=="sas") {
      fext <- "sas7bdat"
    } else {
      fext <- input$dat_exp_type
    }
    fout <- paste0("exportedData_sdcMicro_",format(Sys.time(), "%Y%m%d_%H%M"),".",fext)
    fout <- file.path(obj$path_export, fout)
    cmd <- paste0("writeSafeFile(obj=sdcObj, format=",dQuote(input$dat_exp_type), ", randomizeRecords=",dQuote(input$rb_export_randomizeorder))
    if (input$dat_exp_type=="dta") {
      cmd <- paste0(cmd,", lab=obj$stata_labs")
      cmd <- paste0(cmd,", version=", input$export_dta_version)
    }
    if (input$dat_exp_type=="csv") {
      cmd <- paste0(cmd, ", col.names=",input$export_csv_header)
      cmd <- paste0(cmd, ", sep=",dQuote(input$export_csv_sep))
      cmd <- paste0(cmd, ", dec=",dQuote(input$export_csv_dec))
    }
    cmd <- paste0(cmd,", fileOut=",dQuote(fout),")\n")
    attributes(cmd)$evalAsIs <- TRUE
    return(list(cmd=cmd, path=obj$path_export, fout=fout))
    cmd
  })
  # for use in reproducibility-page
  code_export_sdcproblem <- reactive({
    input$btn_exportProblem # required for timestamp!
    fout <- paste0("exportedProblem_sdcMicro_",format(Sys.time(), "%Y%m%d_%H%M"),".rdata")
    fout <- file.path(obj$path_export, fout)
    cmd <- paste0("save(prob, file=",dQuote(fout), ", compress=TRUE)")
    attributes(cmd)$evalAsIs <- TRUE
    return(list(cmd=cmd, path=obj$path_export, fout=fout))
    cmd
  })
  # for use in undo-page
  code_export_sdcproblem1 <- reactive({
    input$btn_exportProblem1 # required for timestamp!
    fout <- paste0("exportedProblem_sdcMicro_",format(Sys.time(), "%Y%m%d_%H%M"),".rdata")
    fout <- file.path(obj$path_export, fout)
    cmd <- paste0("save(prob, file=",dQuote(fout), ", compress=TRUE)")
    attributes(cmd)$evalAsIs <- TRUE
    return(list(cmd=cmd, path=obj$path_export, fout=fout))
    cmd
  })
  ### END CODE GENERATION EXPRESSIONS ####

  ### EVENTS ###
  # read data if fileInput() has been triggered
  observeEvent(input$file1, {
    obj$microfilename <- input$file1$name
    ptm <- proc.time()
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Loading microdata - please wait", value = 0)
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
      obj$utf8 <- !all(sapply(attr(obj$inputdata, "nonUTF"), is.null))
      code_out <- gsub(input$file1$datapath, input$file1$name, code)
      code_out <- gsub("res", "inputdata", code_out)
      obj$code_read_and_modify <- code_out
      obj$inputdataB <- obj$inputdata
      obj$code_read_and_modify <- c(obj$code_read_and_modify,"inputdataB <- obj$inputdata\n")
      obj$sdcObj <- NULL # start fresh

      # stata
      if (obj$cur_selection_import=="btn_import_data_6") {
        obj$stata_labs <- attributes(obj$inputdata)$lab
        df <- obj$stata_labs[[1]]
        if (!is.null(df)) {
          obj$stata_varnames <- data.frame(var.names=unlist(df[[1]]), var.label=unlist(df[[2]]), stringsAsFactors=FALSE)
        } else {
          obj$stata_varnames <- NULL
        }
      } else {
        obj$stata_labs <- NULL
        obj$stata_varnames <- NULL
      }
      ptm <- proc.time()-ptm
      obj$comptime <- obj$comptime+ptm[3]
    }
  })

  # read a household-level-file
  observeEvent(input$file_hhfile, {
    cmd <- code_importHHFile()
    res <- try(eval(parse(text=cmd)))
    if (class(res)%in%"try-error") {
      obj$last_error <- res
      return(invisible(NULL))
    }
    if (class(res)!="data.frame") {
      obj$last_error <- "object was not a data frame"
      return(invisible(NULL))
    }
    if (length(intersect(colnames(res), colnames(obj$inputdata)))==0) {
      obj$last_error <- "no variables overlap with current loaded microdata"
      return(invisible(NULL))
    }
    obj$hhdata <- res
    obj$last_error <- NULL

    code_out <- gsub(input$file_hhfile$datapath, input$file_hhfile$name, cmd)
    code_out <- gsub("res", "hhdata", code_out)
    obj$code_read_and_modify <- c(obj$code_read_and_modify, "## read household-level data", code_out)
    obj$sdcObj <- NULL # start fresh
  })

  # reset uploaded household-level data
  observeEvent(input$reset_hhdata, {
    obj$hhdata <- NULL
    obj$last_error <- NULL
    obj$code_read_and_modify <- c(obj$code_read_and_modify, "## deleting previously imported household dataset","rm(hhdata)")
  })

  # merge household and individual-level data
  observeEvent(input$btn_merge_hhdata, {
    cmd <- code_merge_hhdata()
    res <- try(eval(parse(text=cmd)))
    if (class(res)%in%"try-error") {
      obj$last_error <- res
      return(invisible(NULL))
    }
    obj$last_error <- NULL
    obj$inputdata <- res # update inputdata
    obj$hhdata_applied <- TRUE
    obj$code_read_and_modify <- c(obj$code_read_and_modify, "## merging household- and individual level microdata",gsub("obj[$]", "", cmd))

    # return to overview
  })

  observeEvent(input$btn_hier_data_prep, {
    cmd <- code_prepare_hhdata()
    res <- try(eval(parse(text=cmd)))
    if (class(res)%in%"try-error") {
      obj$last_error <- res
      return(invisible(NULL))
    }
    obj$last_error <- NULL
    obj$hhdata_selected <- TRUE
    obj$inputdata <- res
    obj$code_read_and_modify <- c(obj$code_read_and_modify, "## restricting input data to household-level data",gsub("obj[$]", "", cmd))
  })

  # use existing data.frame from global environment
  observeEvent(input$btn_chooose_df, {
    ptm <- proc.time()
    cmd <- code_useRObj()
    runEvalStrMicrodat(cmd=cmd, comment=NULL)
    obj$code_read_and_modify <- c(obj$code_read_and_modify,"inputdataB <- obj$inputdata\n")
    obj$inputdataB <- obj$inputdata
    obj$utf8 <- FALSE
    obj$sdcObj <- NULL # start fresh
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    obj$microfilename <- input$sel_choose_df
    obj$stata_labs <- NULL
  })
  # reset variables in inputdataset to their original values
  observeEvent(input$btn_resetmicrovar, {
    ptm <- proc.time()
    cmd <- code_resetmicrovar()
    evalcmd <- gsub("inputdata","obj$inputdata", cmd)
    eval(parse(text=evalcmd))
    obj$code_read_and_modify <- c(obj$code_read_and_modify,"## Reset variable(s) to original state", cmd)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    obj$inp_sel_viewvar1 <- input$sel_reset_microvars[1]
    obj$cur_selection_microdata <- "btn_menu_microdata_2"
  })
  # undo-button

  ## show confirmation modal window before undoing last step
  observeEvent(input$btn_undo_xx, {
    txt <- p("By clicking the button below, you really undo the last anonymization step")
    btn <- myActionButton("btn_undo", label=("Undo last step"), "danger")
    inp <- fluidRow(
      column(12, txt, align="center"),
      column(12, btn, align="center")
    )
    showModal(modalDialog(inp, title="Confirm to undo last anonymization step", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
  })

  observeEvent(input$btn_undo, {
    ptm <- proc.time()
    cmd <- code_undo()
    runEvalStr(cmd=cmd, comment=NULL)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    removeModal(session=session) # remove the modal
    obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
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
    res <- code_globalRecodeMicrodata()
    runEvalStrMicrodat(cmd=res$cmd, comment=res$comment)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    # Reset Input
    updateSelectInput(session, "sel_custom_split", selected="no")
    updateSelectInput(session, "sel_algo", selected="equidistant")
    updateTextInput(session, "txt_custom_breaks", value = "")

    # change reactive var so that overview-page shows the correct variable
    if (input$sel_custom_split=="no") {
      vv <- input$sel_num_glrec[1]
    } else {
      vv <- input$rb_num_glrec
    }
    obj$inp_sel_viewvar1 <- vv
    # Switch to variable-view page
    obj$cur_selection_microdata <- "btn_menu_microdata_2"
  })
  # recode to factor
  observeEvent(input$btn_recode_to_numeric, {
    ptm <- proc.time()
    res <- code_globalRecodeMicrodataToNum()
    runEvalStrMicrodat(cmd=res$cmd, comment=res$comment)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    # change reactive var so that overview-page shows the correct variable
    obj$inp_sel_viewvar1 <- input$sel_to_num_var[1]
    # Switch to variable-view page
    obj$cur_selection_microdata <- "btn_menu_microdata_2"
  })
  # setup the sdcMicroObj
  observeEvent(input$btn_setup_sdc, {
    cmd <- code_createSdcObj()
    cmd_opts <- cmd$update_options
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
    obj$code_setup <- paste0("## Set up sdcMicro object\n",cmd,"\n")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    # if file was uploaded, update filename in options so that it is written to the report
    if (!is.null(obj$microfilename)) {
      eval(parse(text=cmd_opts))
      cmd_opts <- gsub("obj[$]sdcObj","sdcObj", cmd_opts)
      obj$code_setup <- paste0(obj$code_setup, "\n## Store name of uploaded file\n",cmd_opts,"\n")
    }

    # create slider-outputs for k-Anon by group
    # this is required because otherwise these sliders would always be updating
    nrKeyVars <- isolate(length(get_keyVars()))
    obj$sls <- lapply(1:nrKeyVars, function(i) {
      id <- paste0("sl_kanon_combs_", i)
      sliderInput(id, label=p(paste("Set k-anonymity parameter for combinations of", i, "variables")),
        value=input[[id]], width="100%", min=2, max=50, step=1)
    })
    obj$rbs <- lapply(1:nrKeyVars, function(i) {
      id <- paste0("rb_kanon_usecombs_", i)
      radioButtons(id, label=p(paste("Apply k-anonymity to all subsets of",i,"key variables?")),
        selected=input[[id]], width="100%", inline=TRUE, choices=c("No", "Yes"))
    })
    # reset all menu items to first choice
    obj$cur_selection_results <- "btn_results_1"
    obj$cur_selection_exports <- "btn_export_results_1"
    obj$cur_selection_script <- "btn_export_script_1"
    obj$cur_selection_microdata <- "btn_menu_microdata_1"
    obj$cur_selection_import <- "btn_import_data_1"
    obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
    obj$ldiv_result <- NULL # reset ldiversity measure
    obj$suda2_result <- NULL # reset suda2 measure
  })
  # add ghost-vars to an existing sdcMicroObj
  observeEvent(input$btn_addGhostVars, {
    ptm <- proc.time()
    res <- code_addGhostvars()
    runEvalStr(cmd=res$cmd, comment="## Adding linked (ghost)-Variables")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    if (is.null(lastError())) {
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
    obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
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
    obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
  })

  ## show confirmation modal window before resetting the current problem
  observeEvent(input$btn_reset_sdc1, {
    if (!is.null(sdcObj())) {
      txt <- p("By clicking the button below, you delete the current sdcProblem!")
      btn <- myActionButton("btn_reset_sdc",label=("Delete current problem"), "danger")
      inp <- fluidRow(
        column(12, txt, align="center"),
        column(12, btn, align="center")
      )
      showModal(modalDialog(inp, title="Confirm to delete the current sdcProblem", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
    }
  })

  observeEvent(input$btn_acc_utf8_conv, {
    obj$utf8 <- FALSE
  })

  observeEvent(input$btn_reset_sdc, {
    ptm <- proc.time()
    obj$sdcObj <- NULL
    obj$code_anonymize <- c()
    obj$code_setup <- c()
    obj$anon_performed <- NULL
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    removeModal(session=session) # remove the modal
    obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
  })

  ## show confirmation modal window before resetting inputdata
  observeEvent(input$btn_reset_inputdata_xx, {
    if (!is.null(inputdata())) {
      txt <- p("By clicking the button below, you really delete the current inputdata")
      btn <- myActionButton("btn_reset_inputdata",label=("Delete current inputdata"), "danger")
      inp <- fluidRow(
        column(12, txt, align="center"),
        column(12, btn, align="center")
      )
      showModal(modalDialog(inp, title="Confirm to delete the current inputdata", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
    }
  })

  ## show confirmation modal window before resetting inputdata
  observeEvent(input$btn_reset_inputdata_utf8labs_xx, {
    if (!is.null(inputdata())) {
      txt <- p("By clicking the button below, you really delete the current inputdata")
      btn <- myActionButton("btn_reset_inputdata_utf8labs",label=("Delete current inputdata"), "danger")
      inp <- fluidRow(
        column(12, txt, align="center"),
        column(12, btn, align="center")
      )
      showModal(modalDialog(inp, title="Confirm to delete the current inputdata", footer=modalButton("Dismiss"), size="m", easyClose=TRUE, fade=TRUE))
    }
  })

  observeEvent(input$btn_reset_inputdata, {
    #cat(paste("'btn_reset_inputdata' was clicked",input$btn_reset_inputdata,"times..!\n"))
    ptm <- proc.time()
    obj$inputdata <- NULL
    obj$hhdata <- NULL
    obj$hhdata_applied <- FALSE
    obj$hhdata_selected <- FALSE
    obj$sdcObj <- NULL
    obj$code_anonymize <- c()
    obj$code_setup <- c()
    obj$anon_performed <- NULL
    obj$code_read_and_modify <- c()
    obj$utf8 <- FALSE
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    removeModal(session=session) # remove the modal
  })

  observeEvent(input$btn_reset_inputdata_utf8labs, {
    ptm <- proc.time()
    obj$inputdata <- NULL
    obj$hhdata <- NULL
    obj$hhdata_applied <- FALSE
    obj$hhdata_selected <- FALSE
    obj$sdcObj <- NULL
    obj$code_anonymize <- c()
    obj$code_setup <- c()
    obj$anon_performed <- NULL
    obj$code_read_and_modify <- c()
    obj$utf8 <- FALSE
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    removeModal(session=session) # remove the modal
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

  observeEvent(input$btn_reset_inputerror3, {
    ptm <- proc.time()
    obj$last_error <- NULL
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
  })
  # event to generate stratification variable!
  observeEvent(input$btn_create_stratavar, {
    ptm <- proc.time()
    res <- code_generateStrata()
    runEvalStrMicrodat(cmd=res$cmd, comment=res$comment)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    obj$inp_sel_viewvar1 <- input$inp_vname_strata
    # Switch to variable-view page
    obj$cur_selection_microdata <- "btn_menu_microdata_2"
  })
  # event to update/modify an existing factor variable
  observeEvent(input$btn_update_factor, {
    ptm <- proc.time()
    cmd <- code_groupAndRename()
    runEvalStrMicrodat(cmd=cmd, comment="## Modify an existing factor variable")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
  })
  # event to use only a subset of the available microdata
  observeEvent(input$btn_sample_microdata, {
    ptm <- proc.time()
    res <- code_subsetMicrodata()
    runEvalStrMicrodat(cmd=res$cmd, comment=res$comment)
    # Switch to view microdata
    obj$cur_selection_microdata <- "btn_menu_microdata_1"
  })
  # event to set values to na in inputdata
  observeEvent(input$btn_set_to_na, {
    ptm <- proc.time()
    cmd <- code_set_to_na()
    runEvalStrMicrodat_no_errorchecking(cmd=cmd, comment="## Set specific values to 'NA'")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    obj$inp_sel_viewvar1 <- input$sel_na_suppvar[1]
    # Switch to variable-view page
    obj$cur_selection_microdata <- "btn_menu_microdata_2"
  })
  ### anonymization methods (categorical) ###
  # pram() with given transition-matrix
  observeEvent(input$btn_pram_expert, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Performing PRAM - please wait", value = 0)
    ptm <- proc.time()
    res <- code_pram_expert()

    # temporarily set strata-variable
    if (!is.na(res$cmd_strata1)) {
      cmd1 <- res$cmd_strata1
      attributes(cmd1)$evalAsIs <- TRUE
      runEvalStr(cmd=cmd1, comment="## set stratification variable")
      if (is.null(lastError())) {
        runEvalStr(cmd=res$cmd, comment="## Postrandomization (using a transition matrix)")
        cmd2 <- res$cmd_strata2
        if (!is.na(cmd2)) {
          attributes(cmd2)$evalAsIs <- TRUE
          runEvalStr(cmd=cmd2, comment="## reset stratification variable")
        }
      }
    } else {
      runEvalStr(cmd=res$cmd, comment="## Postrandomization (using a transition matrix)")
    }
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    progress$set(message="Performing PRAM - please wait", value = 1)
    if (is.null(lastError())) {
      obj$last_error <- NULL
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
    if (is.null(lastError())) {
      obj$last_error <- NULL
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
    updateRadioButtons(session, "rb_expert_pram", choices=c(FALSE, TRUE), inline=TRUE)
  })
  # pram() with parameters 'pd' and 'alpha'
  observeEvent(input$btn_pram_nonexpert, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Performing PRAM - please wait", value = 0)
    ptm <- proc.time()
    res <- code_pram_nonexpert()
    # temporarily set strata-variable
    if (!is.na(res$cmd_strata1)) {
      cmd1 <- res$cmd_strata1
      attributes(cmd1)$evalAsIs <- TRUE
      runEvalStr(cmd=cmd1, comment="## set stratification variable")
      if (is.null(lastError())) {
        obj$last_error <- NULL
        runEvalStr(cmd=res$cmd, comment="## Postrandomization (using a invariant, randomly generated transition matrix)")
        cmd2 <- res$cmd_strata2
        if (!is.na(cmd2)) {
          attributes(cmd2)$evalAsIs <- TRUE
          runEvalStr(cmd=cmd2, comment="## reset stratification variable")
        }
      }
    } else {
      runEvalStr(cmd=res$cmd, comment="## Postrandomization (using a invariant, randomly generated transition matrix)")
    }
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    progress$set(message="Performing PRAM - please wait", value = 1)
    if (is.null(lastError())) {
      obj$last_error <- NULL
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
  })
  # kAnon()
  observeEvent(input$btn_kanon, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Performing local suppression - please wait", value = 0)
    ptm <- proc.time()
    res <- code_kAnon()
    # temporarily set strata-variable
    if (!is.na(res$cmd_strata1)) {
      cmd1 <- res$cmd_strata1
      attributes(cmd1)$evalAsIs <- TRUE
      runEvalStr(cmd=cmd1, comment="## set stratification variable")
    }
    if (is.null(lastError())) {
      runEvalStr(cmd=res$cmd, comment="## Local suppression to obtain k-anonymity")
      cmd2 <- res$cmd_strata2
      if (!is.na(cmd2)) {
        attributes(cmd2)$evalAsIs <- TRUE
        runEvalStr(cmd=cmd2, comment="## reset stratification variable")
      }
    }
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    progress$set(message="Performing local suppression - please wait", value = 1)
    if (is.null(lastError())) {
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
  })
  # suppress risky observations
  observeEvent(input$btn_supp_th, {
    ptm <- proc.time()
    res <- code_suppThreshold()
    runEvalStr(cmd=res$cmd, comment="## Suppression of risky observations above threshold in specified variable")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    if (is.null(lastError())) {
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
    obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
  })
  # event to update/modify an existing factor variable
  observeEvent(input$btn_update_recfac, {
    ptm <- proc.time()
    res <- code_groupAndRename_keyvar()
    runEvalStr(cmd=res$cmd, comment="## Recode variable")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    if (is.null(lastError())) {
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
  })

  ### anonymization methods (numerical) ###
  # topBotCoding()
  observeEvent(input$btn_topbotcoding_num, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Performing top- or bottom-coding - please wait", value = 0)
    ptm <- proc.time()
    res <- code_topBotCoding_num()
    runEvalStr(cmd=res$cmd, comment="## Performing top/bottom-coding")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    if (is.null(lastError())) {
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
    progress$set(message="Performing top- or bottom-coding - please wait", value = 1)
  })
  # microaggregation()
  observeEvent(input$btn_microagg, {
    # Create a Progress object and close it on exit
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Performing microaggregation - please wait", value = 0)
    ptm <- proc.time()
    res <- code_microaggregation()
    # temporarily set strata-variable
    if (!is.na(res$cmd_strata1)) {
      cmd1 <- res$cmd_strata1
      attributes(cmd1)$evalAsIs <- TRUE
      runEvalStr(cmd=cmd1, comment="## set stratification variable")
    }
    if (is.null(lastError())) {
      runEvalStr(cmd=res$cmd, comment="## Microaggregation")
      cmd2 <- res$cmd_strata2
      if (!is.na(cmd2)) {
        attributes(cmd2)$evalAsIs <- TRUE
        runEvalStr(cmd=cmd2, comment="## reset stratification variable")
      }
    }
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    progress$set(message="Performing microaggregation - please wait", value = 1)
    if (is.null(lastError())) {
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
    obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
  })
  # addNoise()
  observeEvent(input$btn_noise, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Adding noise - please wait", value = 0)
    ptm <- proc.time()
    res <- code_addNoise()
    runEvalStr(cmd=res$cmd, comment="## Adding stochastic noise")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    if (is.null(lastError())) {
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
    progress$set(message="Adding noise - please wait", value = 1)
    obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
  })
  # rankSwap()
  observeEvent(input$btn_rankswap, {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Rank swapping - please wait", value = 0)
    ptm <- proc.time()
    res <- code_rankSwap()
    runEvalStr(cmd=res$cmd, comment="## Performing rankSwapping")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]

    if (is.null(lastError())) {
      obj$lastaction <- res$txt_action
      obj$anon_performed <- c(obj$anon_performed, res$txt_action)
    }
    progress$set(message="Rank swapping - please wait", value = 1)
    obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
  })

  ### risk-measurements ###
  # ldiversity()
  # reset ldiv_result slot; required for better ui
  observeEvent(input$btn_ldiv_restart, {
    obj$ldiv_result <- NULL
  })
  observeEvent(input$btn_ldiv, {
    ptm <- proc.time()
    cmd <- code_ldiv()
    # hack: we do not want the sdcmicro obj to be updated
    # instead we calculate ldivsersity on a data.frame
    cmd <- paste0("## calculating l-diversity measure","\n",cmd)
    #runEvalStr(cmd=cmd, comment="## calculating l-diversity measure")

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Computing l-diversity - please wait", value = 0)

    obj$ldiv_result <- calc_ldiv_result()
    progress$set(message="Computing l-diversity - please wait", value = 1)

    obj$code_anonymize <- c(obj$code_anonymize, cmd)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
  })
  # suda2()
  observeEvent(input$btn_suda2_restart, {
    obj$suda2_result <- NULL
  })
  observeEvent(input$btn_suda2, {
    ptm <- proc.time()
    cmd <- code_suda2()
    # hack: we do not want the sdcmicro obj to be updated
    # instead we calculate ldivsersity on a data.frame
    #runEvalStr(cmd=cmd, comment="## calculating suda2 risk-measure")
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Calculating suda2 scores  - please wait", value = 0)

    obj$suda2_result <- calc_suda2_result()
    progress$set(message="Calculating suda2 scores - please wait", value = 1)

    cmd <- paste0("## calculating suda2 riskmeasure","\n",cmd)
    obj$code_anonymize <- c(obj$code_anonymize, cmd)
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
  })

  # create sdc-report
  observeEvent(input$myRepDownload, {
    ptm <- proc.time()
    res <- code_report()
    runEvalStr(cmd=res$cmd, comment="## creating a sdc-report")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    obj$lastreport <- paste0(file.path(res$path, res$fout),".html")
  })

  # export the anonymized data
  observeEvent(input$btn_export_anon_data, {
    ptm <- proc.time()
    res <- code_export_anondata()

    if (input$dat_exp_type=="dta") {
      newlabs <- obj$stata_labs # original labs
      current_labs <- obj$stata_varnames # after changing the interactive table

      # meta-information is available
      if (!is.null(current_labs)) {
        for (i in 1:nrow(current_labs)) {
          newlabs <- sdcMicro:::changeVarLabel(newlabs, varname=current_labs$var.names[i], newlabel=current_labs$var.label[i])
        }
        obj$stata_labs <- newlabs
      }
    }
    runEvalStr(cmd=res$cmd, comment="## export anonymized data file and save to disk")
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    obj$lastdataexport <- res$fout
  })

  # export an sdcProblem (reproducibility page)
  observeEvent(input$btn_exportProblem, {
    ptm <- proc.time()
    res <- code_export_sdcproblem()
    prob <- reactiveValuesToList(obj, all.names=FALSE)
    class(prob) <- "sdcMicro_GUI_export"
    erg <- try(eval(parse(text=res$cmd)))
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    if (class(erg)!="try-error") {
      obj$lastproblemexport <- res$fout
      obj$last_error <- NULL
    } else {
      obj$last_error <- erg
    }
  })

  # export an sdcProblem (undo page)
  observeEvent(input$btn_exportProblem1, {
    ptm <- proc.time()
    res <- code_export_sdcproblem1()
    prob <- reactiveValuesToList(obj, all.names=FALSE)
    class(prob) <- "sdcMicro_GUI_export"
    erg <- try(eval(parse(text=res$cmd)))
    ptm <- proc.time()-ptm
    obj$comptime <- obj$comptime+ptm[3]
    if (class(erg)!="try-error") {
      obj$lastproblemexport1 <- res$fout
      obj$last_error <- NULL
    } else {
      obj$last_error <- erg
    }
  })

  # import an sdcProblem (reproducibility page)
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
        for (nn in names(res)) {
          obj[[nn]] <- res[[nn]]
        }
        obj$cur_selection_script <- "btn_export_script_1" # was 3 when exporting the problem
        obj$last_error <- NULL
        obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
        updateNavbarPage(session, "mainnav", selected="Anonymize")
      }
    }
  })

  # import an sdcProblem (undo page)
  observeEvent(input$file_importProblem1, {
    ptm <- proc.time()
    code <- code_import_problem1()
    eval(parse(text=code))
    if ("simpleError" %in% class(res)) {
      obj$last_error <- res$message
      updateNavbarPage(session, "mainnav", selected="Undo")
    } else {
      if (!"sdcMicro_GUI_export" %in% class(res)) {
        obj$last_error <- "data read into the system was not of class 'sdcMicro_GUI_export'"
        updateNavbarPage(session, "mainnav", selected="Undo")
      } else {
        for (nn in names(res)) {
          obj[[nn]] <- res[[nn]]
        }
        obj$cur_selection_script <- "btn_export_script_1" # was 3 when exporting the problem
        obj$last_error <- NULL
        rm(res)
        obj$cur_selection_anon <- "btn_sel_anon_1" # jump to summary!
        updateNavbarPage(session, "mainnav", selected="Anonymize")
      }
    }
  })

  # save script to file
  observeEvent(input$btn_save_script, {
    fout <- paste0("exportedScript_sdcMicro_",format(Sys.time(), "%Y%m%d_%H%M"),".R")
    ff <- file.path(obj$path_export, fout)
    cat(paste0(current_code(), collapse="\n"), file=ff)
    obj$lastscriptexport <- ff
  })

  # go to some pages to "undo" in case user wants to upload a previously saved file
  observeEvent(input$nodata_export_uploadproblem, {
    updateNavbarPage(session, "mainnav", selected="Undo")
  })
  observeEvent(input$nodata_results_uploadproblem, {
    updateNavbarPage(session, "mainnav", selected="Undo")
  })
  observeEvent(input$nodata_anonymize_uploadproblem, {
    updateNavbarPage(session, "mainnav", selected="Undo")
  })
  observeEvent(input$nodata_script_uploadproblem, {
    updateNavbarPage(session, "mainnav", selected="Undo")
  })
  observeEvent(input$nodata_script_about, {
    updateNavbarPage(session, "mainnav", selected="Undo")
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
