#' print.chenreg


#' @param x  a model created by `chen_reg`
#' @param ... further arguments passed to or from other methods.
#' @rdname print.chenreg
#' @export print.chenreg
#' @export

print.chenreg <-
  function(x, ...) {
    cat("\nCall:", deparse(x$call, width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")

    cat("\nCoefficients:\n")
    print(structure((as.vector((x$coefficients))),
      .Names = x$names
    ))
  }
