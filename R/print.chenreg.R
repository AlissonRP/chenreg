#' @export print.chenreg
#' @export

print.chenreg <-
  function(x, digits = max(3, getOption("digits") - 3), ...)
  {
    cat("\nCall:", deparse(x$call, width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")

    cat("\nCoefficients:\n")
    print(structure((as.vector((x$coefficients))),
                    .Names = x$names))



  }

