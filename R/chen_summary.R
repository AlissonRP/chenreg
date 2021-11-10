
#' @export
chen_summary <- function(z){
  model_presentation <- cbind(round(z$coef, 4), round(z$stderror, 4), round(z$zstat, 4), round(z$pvalues, 4))
  colnames(model_presentation) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  model_presentation[,4]<-  format.pval(model_presentation[,4], eps = .001, digits = 2)
  as.data.frame(model_presentation)

}
