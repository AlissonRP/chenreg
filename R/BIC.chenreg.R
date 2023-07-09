#' BIC.chenreg

#' @param object  a fitted model created by `chen_reg`,
#' for which there exists the log-likelihood.
#' @param ... further arguments passed to or from other methods.
#'
#' @rdname BIC.chenreg
#' @export BIC.chenreg
#' @export
AIC.chenreg = function(object, ...){
  return(object$metrics$bic)
}
