#' @export residuals.chenreg
#' @export
residuals.chenreg <-
  function(x, summary = FALSE){
  if(summary == T){
    hist(x$residuals, main="Quantile Residuals",
         xlab="Values")
  }
  return(x$residuals)
  }
