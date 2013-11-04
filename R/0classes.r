### class sdcMicroObj ###

#' @useDynLib sdcMicro
#' @import "methods"
#' @import "Rcpp"
#' @import "Rglpk"

setClassUnion('dataframeOrNULL', c('data.frame', 'NULL'))
setClassUnion('numericOrNULL', c('numeric', 'NULL'))
setClassUnion('characterOrNULL', c('character', 'NULL'))
setClassUnion('logicalOrNULL', c('logical', 'NULL'))
setClassUnion('matrixOrNULL', c('matrix', 'NULL'))
setClassUnion('listOrNULL', c('list', 'NULL'))
setClassUnion('factorOrNULL', c('factor', 'NULL'))
setClassUnion('sdcmicroOrNULL', c('NULL'))

#' S4 class describing a sdcMicro-object
#'
#' This class models a data object containing the 'raw' data for a given problem 
#' as well as information on the position of the dimensional variables, the count
#' variable, additional numerical variables, weights or sampling weights within the 
#' raw data. Also slot 'isMicroData' shows if slow 'rawData' consists of microdata
#' (multiple observations for each cell are possible, isMicroData==TRUE) or if data 
#' have already been aggregated (isMicroData==FALSE)
#'
#' \describe{
#' \item{slot \code{rawData}:}{list with each element being a vector of either codes of dimensional variables, counts, weights that should be used for secondary cell suppression problem, numerical variables or sampling weights. }
#' ...
#' }
#' @name sdcMicroObj-class
#' @rdname sdcMicroObj-class
#' @exportClass sdcMicroObj
#' @note objects of class \code{dataObj} are input for slot \code{dataObj} in class \code{sdcProblem}
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
setClass(
	Class='sdcMicroObj', 
	representation=representation(			
		origData='dataframeOrNULL',
		keyVars='numericOrNULL',
		pramVars='numericOrNULL',
		numVars='numericOrNULL',
		weightVar='numericOrNULL',
		hhId='numericOrNULL',
		strataVar='numericOrNULL',
		sensibleVar='numericOrNULL',
		manipKeyVars='dataframeOrNULL',
		manipPramVars='dataframeOrNULL',
		manipNumVars='dataframeOrNULL',
		manipStrataVar='factorOrNULL',
    	originalRisk='listOrNULL',
    	risk='listOrNULL',
		utility='listOrNULL',
		pram='listOrNULL',
		localSuppression='listOrNULL',
		options='listOrNULL',
    	additionalResults='listOrNULL',
		set='listOrNULL',
		prev='sdcmicroOrNULL',
    deletedVars="characterOrNULL"
	),
	prototype=prototype(
		origData=NULL,
		keyVars=NULL,
		pramVars=NULL,
		numVars=NULL,
		weightVar=NULL,
		hhId=NULL,
		strataVar=NULL,
		sensibleVar=NULL,
		manipKeyVars=NULL,
		manipPramVars=NULL,
		manipNumVars=NULL,
		manipStrataVar=NULL,
    	originalRisk=NULL,
    	risk=NULL,
		utility=NULL,
		pram=NULL,
		localSuppression=NULL,
		options=NULL,
    	additionalResults=NULL,
		set=NULL,
		prev=NULL,
    deletedVars=NULL
	),
	validity=function(object) {	
		return(TRUE)
	}
)

setIs("sdcMicroObj", "sdcmicroOrNULL")
