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
    print((x$coefficients))

    cat("---\n")
    cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")

  }
