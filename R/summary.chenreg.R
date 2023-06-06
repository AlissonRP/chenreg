#' summary.chenreg



#' @param object  a model created by `chen_reg`
#' @param ... further arguments passed to or from other methods.

#' @export summary.chenreg
#' @export



summary.chenreg <-
  function(object, ...) {
    object$res <- c(min(object$residuals), quantile(object$residuals, 0.1), quantile(object$residuals, .5), quantile(object$residuals, .75), max(object$residuals))
    object$coefficients <- data.frame(row.names = object$names, Estimate = round(object$coefficients, 4), Std_Error = object$stderror, P_Value = object$pvalues)
    class(object) <- "summary.chenreg"
    object
  }
