#' plot.chenreg


#' @param x  a model created by `chen_reg`
#' @param ... further arguments passed to or from other methods.
#'
#' @export plot.chenreg
#' @export

plot.chenreg <- function(x, ...) {
  data = data.frame(residuals = x$residuals, index = 1:length(x$residuals))
  add_margin = 0.05
  y_lim_sup = max(c(3 + add_margin, max(x$residuals) + add_margin))
  y_lim_inf = min(c(-3 - add_margin, min(x$residuals) - add_margin))


    par(ask=TRUE)
    hist(data$residuals,
         main = "Quantile Residuals",
         xlab = "Values"
    )



    plot(data$residuals,
         main = "Residuals vs Index",
         ylab = "Residuals", ylim=c(y_lim_sup, y_lim_inf))
    abline(2, 0, lty=6)
    abline(-2, 0, lty=6)
    abline(3, 0, lty=2)
    abline(-3, 0, lty=2)



    plot(x$residuals, x$fitted.values,
         xlab = "Residuals",
         ylab = "Fitted Values",
         main = "Residuals vs Fitted Values")

    car::qqPlot(x$residuals, col=c("black"), col.lines = "black",
                ylab = "Residuals", xlab = "Norm Quantiles",
                main = "Confidence Envelope")

    par(ask=FALSE)

  }






