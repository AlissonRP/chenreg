#' @export summary.chenreg
#' @export



summary.chenreg <-
  function(x){




    x$res = c(min(x$residuals), quantile(x$residuals, 0.1), quantile(x$residuals, .5), quantile(x$residuals, .75), max(x$residuals))
    x$coefficients = data.frame(row.names = x$names, Estimate = x$coefficients, Std_Error = rep("No Sei", length(x$names)), P_Value = rep("No Sei", length(x$names)))
    class(x) <- "summary.chenreg"
    x

  }
