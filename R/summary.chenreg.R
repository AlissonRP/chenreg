#' summary.chenreg



#' @param object  a model created by `chen_reg`
#' @param format print the data.frame of coefficients
#' @param ... further arguments passed to or from other methods.

#' @export summary.chenreg
#' @export



summary.chenreg <-
  function(object, format = FALSE,...) {
    object$format = format
    object$res <- c(min(object$residuals), quantile(object$residuals, 0.1), quantile(object$residuals, .5), quantile(object$residuals, .75), max(object$residuals))
    object$coefficients <- data.frame(row.names = object$names, Estimate = round(object$coefficients, 4), Std_Error = object$stderror,
                                      Z_value = object$zstat, P_Value = object$pvalues)

    object$coefficients[, 4] <- format.pval(object$coefficients[, 4],
                                           eps = .001,
                                           digits = 2)
    if(format == T){
      return(object$coefficients)
    }
    else {
      class(object) <- "summary.chenreg"
      object
    }

  }
