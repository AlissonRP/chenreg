

#' best_chen
#'
#'This function is useful when a well-optimized combination of variables
#'is already known (by `auto_chen`) and the goal is to fit the model using only
#' those selected covariates
#'
#' @param auto_chen  a model created by `auto_chen`





#' @export
best_chen = function(auto_chen){
  quantile = auto_chen$tau
  y = auto_chen$y
  data = auto_chen$data

  formula = paste(y , "~", auto_chen$best) |> as.formula()

  model = chen_reg(data, formula, quantile)
  return(model)


}
