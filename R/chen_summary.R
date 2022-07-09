#' chen_summary
#'
#' Create a formatted summary of the model in the format of `data.frame`
#'
#' @param z  a model created by `chen_reg`
#'
#'
#' @examples
#'
#' modell <- chenReg::chen_reg(data = simu[, -1], formula = Y ~ ., tau = 0.5, link = "log")
#' modell |>
#'   chenReg::chen_summary()
#' @note
#' This is useful to create formatted tables using functions like kable from
#' knitr
#' @export
chen_summary <- function(z) {
  model_presentation <- cbind(round(z$coef, 4), round(z$stderror, 4), round(z$zstat, 4), round(z$pvalues, 4))
  colnames(model_presentation) <- c("estimate", "std_error", "z_value", "p_value")
  model_presentation[, 4] <- format.pval(model_presentation[, 4], eps = .001, digits = 2)
  return(as.data.frame(model_presentation))
}

