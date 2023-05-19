#' @export predict.generalreg
#' @export
#'
predict.generalreg <-
  function(x,  data = NULL){
    if(is.null(data)){
      attach(x$X)
      return(eval_mu(x))
    } else {
      attach(data)
      return(eval_mu(x))
    }
}
