#' fitted.chenreg

#' @param object an object for which the extraction of model fitted values is meaningful.
#' @param ... further arguments passed to or from other methods.
#'
#' @rdname fitted.chenreg
#' @export fitted.chenreg
#' @export
fitted.chenreg = function(object, ...){
  return(object$fitted.values)
}
