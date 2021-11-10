
#' @export
chen_summary <- function(z){
  model_presentation <- cbind(round(z$coef, 4), round(z$stderror, 4), round(z$zstat, 4), round(z$pvalues, 4))
  colnames(model_presentation) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  as.data.frame(model_presentation)

}
