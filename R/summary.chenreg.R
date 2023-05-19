#' @export summary.chenreg
#' @export



summary.chenreg <-
  function(x){




    x$res = c(min(x$residuals), quantile(x$residuals, 0.1), quantile(x$residuals, .5), quantile(x$residuals, .75), max(x$residuals))
    x$coefficients = data.frame(row.names = x$names, Estimate = round(x$coefficients, 4), Std_Error = x$stderror, P_Value = x$pvalues)
    class(x) <- "summary.chenreg"
    x

  }
