#' residuals.chenreg


#' @param object  a model created by `chen_reg`
#' @param ... further arguments passed to or from other methods.
#' @rdname residuals.chenreg
#' @export residuals.chenreg
#' @export
residuals.chenreg <-
  function(object, ...) {
    return(object$residuals)
  }
