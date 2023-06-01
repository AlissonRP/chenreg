#' @export predict.chenreg
#' @export
#'
predict.chenreg <-

  function(x,  data = NULL){
    X = model.matrix(x$formula, data)
    if(is.null(data) == TRUE){
      return(x$fitted.values)
    }
    etahat <- X %*% as.matrix(x$coefficients[2:length(x$coefficients)])
    muhat <- exp(etahat)
    return(muhat)
}
