#' addGhostVars
#'
#' specify variables that are \code{linked} to a key variable. This results in all
#' suppressions of the key-variable being also applied on the corresponding 'ghost'-variables.
#'
#' @name addGhostVars
#' @docType methods
#' @param obj an object of class \code{\link{sdcMicroObj-class}}
#' @param keyVar character-vector of length 1 refering to a categorical key variable within \code{obj}.
#' @param ghostVars a character vector specifying variables that are linked to \code{keyVar}. Variables listed here must not be be listed in either slots
#' \code{@keyVars}, \code{@numVars}, \code{@pramVars}, \code{@weightVar}, \code{@hhId} or \code{@strataVar} in \code{obj}.
#' @return a modified \code{\link{sdcMicroObj-class}} object.
#' @author Bernhard Meindl
#' @references 
#' Templ, M. Statistical Disclosure Control for Microdata: Methods and Applications in R.
#' \emph{Springer International Publishing}, 287 pages, 2017. ISBN 978-3-319-50272-4. \doi{10.1007/978-3-319-50272-4}
#' \doi{10.1007/978-3-319-50272-4}
#' @keywords manip
#' @export
#' @examples
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' ## we want to link the anonymization status of key variabe 'urbrur' to 'hhcivil'
#' sdc <- addGhostVars(sdc, keyVar="urbrur", ghostVars=c("hhcivil"))
#' ## we want to link the anonymization status of key variabe 'roof' to 'represent'
#' sdc <- addGhostVars(sdc, keyVar="roof", ghostVars=c("represent"))
addGhostVars <- function(obj, keyVar, ghostVars) {
  addGhostVarsX(obj, keyVar, ghostVars)
}

setGeneric("addGhostVarsX", function(obj, keyVar, ghostVars) {
  standardGeneric("addGhostVarsX")
})

setMethod(f="addGhostVarsX", signature=c(obj="sdcMicroObj", keyVar="character", ghostVars="character"),
definition = function(obj, keyVar, ghostVars) {
  obj <- nextSdcObj(obj)
  cn <- colnames(get.sdcMicroObj(obj, type="origData"))
  kv <- cn[get.sdcMicroObj(obj, type="keyVars")]
  if (length(keyVar)!=1) {
    stop("length of argument 'keyVar' must be 1 in addGhostVars!\n")
  }
  if (!keyVar %in% kv) {
    stop("variable specified in 'keyVar' in not a categorical key variable!\n")
  }

  nv <- cn[get.sdcMicroObj(obj, type="numVars")]
  pv <- cn[get.sdcMicroObj(obj, type="pramVars")]
  wv <- cn[get.sdcMicroObj(obj, type="weightVar")]
  hhid <- cn[get.sdcMicroObj(obj, type="hhId")]
  sv <- cn[get.sdcMicroObj(obj, type="strataVar")]
  non_poss <- c(kv, nv, pv, wv, hhid, sv)

  gv <- get.sdcMicroObj(obj, type="ghostVars")
  if (!is.null(gv)) {
    ex_gv <- cn[unlist(sapply(gv, function(x) {
      x[[2]]
    }))]
    non_poss <- c(non_poss, ex_gv)
  }
  non_poss <- unique(non_poss)

  if (any(ghostVars %in% non_poss)) {
    stop("variables listed in 'ghostVars' were either specified as important (key) variables or were already specified as ghost-variables!\n")
  }

  new_kv <- standardizeInput(obj=obj, v=keyVar)
  new_gv <- standardizeInput(obj=obj, v=ghostVars)

  if (is.null(gv)) {
    tmp <- list()
    tmp[[1]] <- new_kv
    tmp[[2]] <- new_gv
    inp <- list()
    inp[[1]] <- tmp
  } else {
    inp <- gv
    ii <- which(unlist(sapply(gv, function(x) {
      x[[1]]
    })) == new_kv)

    # is keyVar already specfied -> we add the ghost var
    if (length(ii)==1) {
      inp[[ii]][[2]] <- c(inp[[ii]][[2]], new_gv)
    } else {
      tmp <- list()
      tmp[[1]] <- new_kv
      tmp[[2]] <- new_gv
      inp[[length(inp)+1]] <- tmp
    }
  }
  
  # update/initialize slot manipGhostVars
  manipGhostVars <- get.sdcMicroObj(obj, type = "manipGhostVars")
  if (is.null(manipGhostVars)) {
    manipGhostVars <- get.sdcMicroObj(obj, "origData")[, new_gv, drop = FALSE]
  } else {
    df <- get.sdcMicroObj(obj, "origData")[, new_gv, drop = FALSE]
    new <- setdiff(colnames(df), colnames(manipGhostVars))
    if (length(new) > 0) {
      df <- df[, new, drop = FALSE]
      manipGhostVars <- cbind(manipGhostVars, df)
    }
  }
  obj <- set.sdcMicroObj(obj, type="manipGhostVars", input=list(manipGhostVars))
  obj <- set.sdcMicroObj(obj, type="ghostVars", input=list(inp))
  obj
})
