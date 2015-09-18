setMethod(f = "show", signature = "sdcMicroObj",
definition = function(object) {
  print(object, type="general")

  if (length(object@keyVars) > 0) {
    print(object, type="recode")
    print(object, type="kAnon")
  }
  if (length(object@numVars) > 0) {
    print(object, type="numrisk")
  }
  if ( !is.null(object@pram) ) {
    print(object, type="pram")
  }
  if ( !is.null(object@localSuppression) ) {
    print(object, type="ls")
  }
})
