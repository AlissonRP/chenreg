#' predict.chenreg

#' @param object  a model created by `chen_reg`
#' @param newdata An optional data frame in which to look for variables
#' with which to predict. If omitted, the fitted values are used
#' @param ... further arguments passed to or from other methods.
#'
#' @rdname predict.chenreg
#' @export predict.chenreg
#' @export
#'
predict.chenreg <-
  function(object, newdata = NULL, ...) {
    formula = object$formula

    newdata[as.character(formula[[2]])] = 2
    if (is.null(newdata) == TRUE) {
      return(object$fitted.values)
    }

    X <- model.matrix(formula, newdata)
    etahat <- X %*% as.matrix(object$coefficients[2:length(object$coefficients)])
    muhat <- exp(etahat)
    return(muhat)
  }

