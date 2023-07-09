#' model.matrix.chenreg

#' @param object  a fitted model created by `chen_reg`,
#' for which there exists the log-likelihood.
#' @param ... further arguments passed to or from other methods.
#'
#' @rdname model.matrix.chenreg
#' @export model.matrix.chenreg
#' @export
model.matrix.chenreg = function(object, ...){
  return(object$X)
}
