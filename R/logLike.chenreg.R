#' logLike.chenreg

#' @param object  a fitted model created by `chen_reg`,
#' for which there exists the log-likelihood.
#' @param ... further arguments passed to or from other methods.
#'
#' @rdname logLike.chenreg
#' @export logLike.chenreg
#' @export
logLike.chenreg = function(object, ...){
  return(object$loglik)
}
