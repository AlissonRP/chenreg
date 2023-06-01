
auto.chen = function(y, data, minimize = "rmse" , alpha = 0.05){
  x = data[,!(names(data) %in% y)] |> names()

  combinations = do.call("c", lapply(seq_along(x), \(i) combn(x, i, FUN = list)))
  results = data.frame("variables" = "y", "rmse" = 1,
                       "AIC" = 1,
                       "BIC" = 2,
                       "r-squared" = 2,
                       "significants" = 1,
                       "normal_residuals" = "ok")
  for (i in 1:length(combinations)){
    X = data[combinations[i] |> unlist()]
    Y = data[y]

    models = chen_reg(data.frame(Y, X), Y ~ .)

    vars = paste(X |> names(), collapse = ' + ')
    significants = (\(x) x < alpha) (models$pvalues) |> sum()
    normal_test = (models$residuals |> shapiro.test())$p.value
    check_residuals = (ifelse(normal_test > 0.05, "OK", 'not-normal'))

    results = results |> rbind(data.frame("variables" = vars,
                                          "rmse" = models$metrics$rmse,
                                          "AIC" = models$metrics$aic,
                               "BIC" = models$metrics$bic,
                               "r-squared" = models$metrics$r2,
                               "significants" = significants - 1,
                               "normal_residuals" = check_residuals
                               ))

}
results = results[-1, ]

return(results)

}


