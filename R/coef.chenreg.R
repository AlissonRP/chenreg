#' coef.chenreg

#' @param object an object for which the extraction of model coefficients is meaningful.
#' @param ... further arguments passed to or from other methods.
#'
#' @rdname coef.chenreg
#' @export coef.chenreg
#' @export
coeff.chenreg = function(object, ...){
  return(object$coefficients)
}
