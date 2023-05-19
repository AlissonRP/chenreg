#' @export print.summary.chenreg
#' @export
#'
#'
print.summary.chenreg <-
  function(x, digits = max(3, getOption("digits") - 3), ...)
  {
    cat("\nCall:\n",
        paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")


    cat("\nResiduals:\n")
    print(structure((as.vector((x$res))),
                    .Names = c("Min", "1Q", "Median", "3Q", "Max")))

    cat("\nCoefficients:\n")
    print(structure((x$coefficients)))

    cat("---\n")
    cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")

    cat("\n")

    cat(paste0("Log-likelihood: ", round(x$loglik, 4)), "\n")
    cat(c("Number of iterations in BFGS optim:", x$counts), "\n")
    cat(
      c("AIC:", round(x$metrics$aic, 4), " BIC:", round(x$metrics$bic, 4)),
      " RMSE:", round(x$metrics$rmse, 4), "\n"
    )
    cat(c("Pseudo R-squared:", round(x$metrics$r2, 4)))
  }


