


#' @export
best_chen = function(auto_chen){
  quantile = auto_chen$quantile
  y = auto_chen$y
  data = auto_chen$data

  formula = paste(y , "~", auto_chen$best) |> as.formula()

  model = chen_reg(data, formula, quantile)
  return(model)


}
