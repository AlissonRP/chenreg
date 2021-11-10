
#' @export
chen_summary <- function(z){
  model_presentation <- cbind(round(z$coef, 4), round(z$stderror, 4), round(z$zstat, 4), round(z$pvalues, 4))
  colnames(model_presentation) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
<<<<<<< HEAD
  model_presentation[,4]<-  format.pval(model_presentation[,4], eps = .001, digits = 2)
=======
>>>>>>> 498dd448a5f475f96be3e7bd0e53fc07db7791a9
  as.data.frame(model_presentation)

}
