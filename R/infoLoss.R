#' Calculate information loss after targeted record swapping
#'
#' @description Calculate information loss after targeted record swapping using both the original and the swapped micro data.
#' Information loss will be calculated on table counts defined by parameter `table_vars` using either implemented information loss measures like absolute deviaton, relative absolute deviation and absolute deviation of square roots or custom metric, See details below.
#' 
#' @param data original micro data set, must be either a `data.table` or `data.frame`. 
#' @param data_swapped micro data set after targeted record swapping was applied. Must be either a `data.table` or `data.frame`. 
#' @param table_vars column names in both `data` and `data_swapped`. Defines the variables over which a (multidimensional) frequency table is constructed.
#' Information loss is then calculated by applying the metric in `metric` and `custom_merics` over the cell-counts and margin counts of the table from `data` and `data_swapped`. 
#' @param metric character vector containing one or more of the already implemented metrices: "absD","relabsD" and/or "abssqrtD".
#' @param custom_metric function or (named) list of functions. Functions defined here must be of the form `fun(x,y,...)`
#' where `x` and `y` expect numeric values of the same length. The output of these functions must be a numeric vector of the same length as `x` and `y`.  
#' @param hid `NULL` or character containing household id in `data` and `data_swapped`. If not `NULL` frequencies will reflect number of households, otherwise frequencies will reflect number of persons. 
#' @param probs numeric vector containing values in the inervall [0,1].
#' @param quantvals optional numeric vector which defines the groups used for the cumulative outputs. Is applied on the results `m` from each information loss metric as `cut(m,breaks=quantvals,include.lowest=TRUE)`, see also return values.  
#' @param apply_quantvals character vector defining for the output of which metrices `quantvals` should be applied to. 
#' @param exclude_zeros `TRUE` or `FALSE`, if `TRUE` 0 cells in the frequency table using `data_swapped` will be ignored.
#' @param only_inner_cells `TRUE` or `FALSE`, if `TRUE` only inner cells of the frequency table defined by `table_vars` will be compared. Otherwise also all tables margins will bei calculated.
#'
#' @details First frequency tables are build from both `data` and `data_swapped` using the variables defined in `table_vars`. By default also all table margins will be calculated, see parameter `only_inner_cells = FALSE`.
#' After that the information loss metrices defined in either `metric` or `custom_metric` are applied on each of the table cells from both frequency tables.
#' This is done in the sense of `metric(x,y)` where `metric` is the information loss, `x` a cell from the table created from `data` and `y` the same cell from the table created from `data_swapped`. 
#' One or more custom metrices can be applied using the parameter `custom_metric`, see also examples.  
#'
#' @return Returns a list containing:
#' 
#' * `cellvalues`: `data.table` showing in a long format for each table cell the frequency counts for `data` ~ `count_o` and `data_swapped` ~ `count_s`. 
#' * `overview`: `data.table` containing the disribution of the `noise` in number of cells and percentage. The `noise` ist calculated as the difference between the cell values of the frequency table generated from the original and swapped data
#' * `measures`: `data.table` containing the quantiles and mean (column `waht`) of the distribution of the information loss metrices applied on each table cell. The quantiles are defined by parameter `probs`.
#' * `cumdistr\*`: `data.table` containing the cumulative distribution of the information loss metrices. Distribution is shown in number of cells (`cnt`) and percentage (`pct`). Column `cat` shows all unique values of the information loss metric or the grouping defined by `quantvals`.    
#' * `false_zero`: number of table cells which are non-zero when using `data` and zero when using `data_swapped`.
#' * `false_nonzero`: number of table cells which are zero when using `data` and non-zero when using `data_swapped`.
#' * `exclude_zeros`: value passed to `exclude_zero` when calling the function.
#' 
#' @export infoLoss
#'
#' @examples
#' # generate dummy data 
#' seed <- 2021
#' set.seed(seed)
#' nhid <- 10000
#' dat <- createDat( nhid )
#' 
#' # define paramters for swapping
#' k_anonymity <- 1
#' swaprate <- .05
#' similar <- list(c("hsize"))
#' hier <- c("nuts1","nuts2")
#' carry_along <- c("nuts3","lau2")
#' risk_variables <- c("ageGroup","national")
#' hid <- "hid"
#' 
#' # apply record swapping
#' dat_s <- recordSwap(data = dat, hid = hid, hierarchy = hier,
#'                     similar = similar, swaprate = swaprate,
#'                     k_anonymity = k_anonymity,
#'                     risk_variables = risk_variables,
#'                     carry_along = carry_along,
#'                     return_swapped_id = TRUE,
#'                     seed=seed)
#' 
#' 
#' # calculate informationn loss
#' # for the table nuts2 x national
#' iloss <- infoLoss(data=dat, data_swapped = dat_s,
#'                   table_vars = c("nuts2","national"))
#' iloss$measures # distribution of information loss measures
#' iloss$false_zero # no false zeros
#' iloss$false_nonzero # no false non-zeros
#' 
#' # frequency tables of households accross
#' # nuts2 x hincome
#' 
#' iloss <- infoLoss(data=dat, data_swapped = dat_s,
#'                   table_vars = c("nuts2","hincome"),
#'                   hid = "hid")
#' iloss$measures  
#' 
#' # define custom metric
#' squareD <- function(x,y){
#'   (x-y)^2
#' }
#'
#' iloss <- infoLoss(data=dat, data_swapped = dat_s,
#'                  table_vars = c("nuts2","national"),
#'                  custom_metric = list(squareD=squareD))
#' iloss$measures # includes custom loss as well
#' 

infoLoss <- function(data, data_swapped, table_vars, 
                     metric = c("absD","relabsD","abssqrtD"),
                     custom_metric=NULL, hid = NULL,
                     probs = sort(c(seq(0,1,by=.1),.95,.99)),
                     quantvals = c(0, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, Inf),
                     apply_quantvals = c("relabsD","abssqrtD"),
                     exclude_zeros = FALSE, only_inner_cells = FALSE){
  
  
  . <- count_o <- count_s <- noise <- pct <- cnt <- NULL 
  
  ####################
  # check inputs
  if(!any(class(data) %in% c("data.table","data.frame"))){
    stop("`data` musst be a data.table or data.frame")
  }
  if(!any(class(data_swapped) %in% c("data.table","data.frame"))){
    stop("`data_swapped` musst be a data.table or data.frame")
  }
  setDT(data)
  setDT(data_swapped)
  
  if(any(!table_vars%in%colnames(data))){
    stop("not all variables in `table_vars` are column names of `data`")
  }
  if(any(!table_vars%in%colnames(data_swapped))){
    stop("not all variables in `table_vars` are column names of `data_swapped`")
  }
  
  if(any(!metric%in%c("absD","relabsD","abssqrtD"))){
    stop("`metric` can only take values \"absD\", \"relabsD\" and \"abssqrtD\".\nUse parameter `custom_metric` if you want to apply additional metrices.")
  }
  
  if(!is.null(custom_metric)){
    if(is.function(custom_metric)){
      custom_metric <- list(customD1=custom_metric())
    }else if(is.list(custom_metric)){
      check_fun <- sapply(custom_metric,is.function)
      if(any(!check_fun)){
        stop("If `custom_metric` is a (named) list each list entry must contain a single function")
      }
      
      check_fun2 <- lapply(custom_metric,function(z){
        out <- tryCatch(
          {
            z(1:10,10:1) # check metric using a simple imput
          },
          error=function(cond) {
            return(cond)
          },
          warning=function(cond) {
            return(cond)
          })    
      })
      for(j in seq_along(check_fun2)){
        fun_j <- check_fun2[[j]]
        if(methods::is(fun_j,"simpleWarning")){
          stop("`custom_metric[[",j,"]]` threw a warning when calling `custom_metric[[",j,"]]`(1:10,10:1): ",fun_j$message)
        }
        if(methods::is(fun_j,"simpleError")){
          stop("`custom_metric[[",j,"]]` threw an error when calling `custom_metric[[",j,"]]`(1:10,10:1): ",fun_j$message)
        }
        if(!(is.numeric(fun_j)&&length(fun_j))){
          stop("Function in `custom_metric[[",j,"]]` must return a numeric vector of the same length as the input vectors")
        }
      }
      # set names for custom metric if necessary
      if(is.null(names(custom_metric))){
        names(custom_metric) <- paste0("customD",1:length(custom_metric))
      }
      cn <- names(custom_metric)
      cn_new <- paste0("customD",1:length(custom_metric))
      cn_new <- cn_new[!cn_new%in%cn]
      cn[cn==""] <- cn_new[1:sum(cn=="")]
      names(custom_metric) <- cn
      
    }else{
      stop("`custom_metric` can be either a (named) list of functions or a single function.")
    }    
  }
    
  if(!is.null(hid)){
    if(!all(is.character(hid) & length(hid)==1 & hid %in% colnames(data) & hid %in% colnames(data_swapped))){
      stop("`hid` must be a character vector of length 1 containing a column name in `data` and `data_swapped`")
    }
    
    data <- unique(data,by=c(hid))
    data_swapped <- unique(data_swapped,by=c(hid))
  }
  
  if(!(is.numeric(probs) & all(probs%between%c(0,1)))){
    stop("`probs` must be a numeric vector with values in [0,1]")
  }
  
  if(!all(is.numeric(quantvals) & length(quantvals)>1)){
    stop("`quanvals` must be a numeric vector of length 2 or more")
  }
  if(!is.character(apply_quantvals)){
    stop("`apply_quantvals` must be a character vector containing names of the metric supplied in `metric` or `custom_metric`")
  }
  
  if(!all(is.logical(exclude_zeros) & length(exclude_zeros)==1)){
    stop("`exclude_zeros` must be TRUE or FALSE")
  }
  
  if(!all(is.logical(only_inner_cells) & length(only_inner_cells)==1)){
    stop("`only_inner_cells` must be TRUE or FALSE")
  }
  
  ####################
  # calculate tables
  if(only_inner_cells){
    # calculate counts for combination of all variables only
    comb_vars <- table_vars
  }else{
    # calculate also all table margins
    comb_vars <- do.call(c, lapply(seq_along(table_vars), utils::combn, x = table_vars, simplify = FALSE))
  }
  
  # count variables
  tab_orig <- lapply(comb_vars,function(z){
    data[,.(count_o=.N),by=c(z)]
  })
  tab_orig <- rbindlist(tab_orig,use.names=TRUE,fill=TRUE)
  tab_swap <- lapply(comb_vars,function(z){
    data_swapped[,.(count_s=.N),by=c(z)]
  })
  tab_swap <- rbindlist(tab_swap,use.names=TRUE,fill=TRUE)
  
  # merge results
  tab_count <- merge(tab_orig,tab_swap,all=TRUE,by=table_vars)
  tab_count[is.na(count_o),count_o:=0]
  tab_count[is.na(count_s),count_s:=0]
  
  if(exclude_zeros){
    tab_count <- tab_count[count_s!=0]
  }
  
  dt_overview <- tab_count[,.(cnt=.N,pct=.N/nrow(tab_count)),by=.(noise=count_o-count_s)]
  setorder(dt_overview,noise)
  
  # estimate metric
  n_met <- metric
  metric_possible <- list(absD=absD,
                          relabsD=relabsD,
                          abssqrtD=abssqrtD)
  metric <- metric_possible[metric]
  names(metric) <- n_met
  
  if(!is.null(custom_metric)){
    metric <- c(metric,custom_metric)
    n_met <- names(metric)
  }

  # apply metric
  dt_measures <- list()
  cumdistr_d <- list()
  for(m in n_met){
    
    x_d <- tab_count[,metric[[m]](count_o,count_s)]
    dt_measures <- c(dt_measures,
                     list(get_dist(x_d,probs=probs)))
    
    
    if(m%in%apply_quantvals){
      cumsum_x <- cumsum(table(cut(x_d, quantvals, include.lowest = TRUE)))
      group_1 <- gsub(",.*",",",names(cumsum_x[1]))
      new_names <- names(cumsum_x)[-1]
      names(cumsum_x)[-1] <- gsub("\\(.*,",group_1,new_names)
    }else{
      cumsum_x <- cumsum(table(x_d))
    }
    cumsum_x <- data.table(cat = names(cumsum_x), cnt = cumsum_x)
    
    cumsum_x[,pct:=cnt/length(x_d)]
    cumdistr_d <- c(cumdistr_d,list(cumsum_x))
  }
  names(dt_measures) <- n_met
  names(cumdistr_d) <- paste0("cumdistr",n_met)
  dt_measures <- rbindlist(dt_measures,idcol = "d")
  dt_measures <- dcast(dt_measures,what~d,value.var="x_d",fill=NA)
  
  # cells that were perturbed to zero
  false_zero <- tab_count[count_o!=0 & count_s==0,.N]
  # zeros that were perturbed to a value !=0
  false_nonzero <- tab_count[count_o==0 & count_s!=0,.N]
  
  return(
    c(list(
      cellvalues = tab_count,
      overview = dt_overview[],
      measures = dt_measures[]),
      cumdistr_d,
      list(false_zero = false_zero,
      false_nonzero = false_nonzero,
      exclude_zeros = exclude_zeros)
    )
  )
}

# help functions for information loss
get_dist <- function(x,probs=probs){
  what <- NULL
  x_d <- stats::quantile(x,probs=probs)
  x_names <- names(x_d)
  x_names[x_names=="0%"] <- "Min"
  x_names[x_names=="50%"] <- "Median"
  x_names[x_names=="100%"] <- "Max"
  x_m <- mean(x)
  pos_m <- max(which(probs<.5))
  x_d <- data.table(what=x_names,x_d)
  x_d <- rbind(x_d[1:(pos_m)],
               data.table(what="Mean",x_m),
               x_d[-c(1:(pos_m))],use.names=FALSE)
  x_d[,what:=factor(what,levels=what)]
  return(x_d)
}

# help functions for metric
absD <- function(x,y){
  abs(x-y)
}
relabsD <- function(x,y){
  x <- abs(x-y)/x
  x[is.infinite(x)] <- 0
  return(x)
} 
abssqrtD <- function(x,y){
  abs(sqrt(x)-sqrt(y))
} 

