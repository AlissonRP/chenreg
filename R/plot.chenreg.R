#' plot.chenreg


#' @param x  a model created by `chen_reg`
#' @param ... further arguments passed to or from other methods.
#' @param titles Title to appear above the plots
#' @export plot.chenreg
#' @export

plot.chenreg <- function(x, titles = TRUE,...) {
  data = data.frame(residuals = x$residuals, index = 1:length(x$residuals))



  main_plots = c("Quantile Residuals",
             "Residuals vs Index",
             "Residuals vs Fitted Values",
             "Confidence Envelope")

  if (titles == FALSE){
    main_plots = as.vector(rep("", 4))
  }


  add_margin = 0.05
  y_lim_sup = max(c(3 + add_margin, max(x$residuals) + add_margin))
  y_lim_inf = min(c(-3 - add_margin, min(x$residuals) - add_margin))


    par(ask = TRUE)

    hist(data$residuals,
         main = main_plots[1],
         xlab = "Values"
    )



    plot(data$residuals,
         main = main_plots[2],
         ylab = "Residuals", ylim=c(y_lim_sup, y_lim_inf))
    abline(2, 0, lty=6)
    abline(-2, 0, lty=6)
    abline(3, 0, lty=2)
    abline(-3, 0, lty=2)



    plot(x$residuals, x$fitted.values,
         xlab = "Residuals",
         ylab = "Fitted Values",
         main = main_plots[3])

    car::qqPlot(x$residuals, col=c("black"), col.lines = "black",
                ylab = "Residuals", xlab = "Norm Quantiles",
                main = main_plots[4])

    par(ask=FALSE)

  }






