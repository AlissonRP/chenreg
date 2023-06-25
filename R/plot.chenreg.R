#' plot.chenreg


#' @param x  a model created by `chen_reg`
#' @param ... further arguments passed to or from other methods.
#'
#' @export plot.chenreg
#' @export

plot.chenreg <- function(x, ...) {
  data = data.frame(residuals = x$residuals, index = 1:length(x$residuals))
    par(ask=TRUE)
    hist(data$residuals,
         main = "Quantile Residuals",
         xlab = "Values"
    )
    plot(data$residuals,
         main = "Residuals vs Index",
         ylab = "Residuals")

    plot(x$residuals, x$fitted.values,
         xlab = "Residuals",
         ylab = "Fitted Values",
         main = "Residuals vs Fitted Values")

    car::qqPlot(x$residuals, col=c("black"), col.lines = "black",
                ylab = "Residuals", xlab = "Norm Quantiles")

    par(ask=FALSE)

  }