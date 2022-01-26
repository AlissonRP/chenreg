#' chen_summary
#'
#' Create a summary of the model in the format of `data.frame`
#'
#' @param z  a model created by chen_reg.fit
#'
#' @export
#'
#' @examples
#'
#' modell <- chenReg::chen_reg.fit(Y ~ ., data = simu[, -1], tau = 0.5, link = "log")
#' modell |>
#'   chen_summary()
chen_summary <- function(z) {
  model_presentation <- cbind(round(z$coef, 4), round(z$stderror, 4), round(z$zstat, 4), round(z$pvalues, 4))
  colnames(model_presentation) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  model_presentation[, 4] <- format.pval(model_presentation[, 4], eps = .001, digits = 2)

  return(as.data.frame(model_presentation))
}
