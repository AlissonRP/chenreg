
#' auto_chen
#'
#' auto_chen is used to find the best combinations of exogenous variables based
#' on the metrics that you interested

#'
#' @param y 	is the name of the target variable:
#'
#' @param  data   data frame, list or environment (or object coercible by
#' as.data.frame to a data frame)
#'                containing the variables in the model.
#' @param metric     metric you are interested in
#'
#' @param alpha significance coefficient in hypothesis testing
#'
#' @param quantile     a number that indicates the quantile that you want to fit
#' the regression
#'
#' @param total number of combinations
#'
#' @param info if is TRUE will print in console the best combination of variables
#'
#' @examples
#' library(chenreg)
#' data <- simu[, -1]
#' auto_chen("Y", data, metric = "bic", info = T)
#'
#'
#' @note
#' You can specify all variables (except y) to be your covariates using `.`, you
#' can also add transformation like `log(x1)`
#' You can write -1 in the formula to not use intercept in the model.
#' @return chenReg returns an object of class `chenreg`







auto_chen <- function(y, data, metric = "rmse", quantile = 0.5, alpha = 0.05, total = NULL, info = FALSE) {
  x_names <- data[, !(names(data) %in% y)] |> names()




  metric <- tolower(metric)

  max_combinations <- do.call("c", lapply(seq_along(x_names), \(i) combn(x_names, i, FUN = list)))

  if (is.null(total) == TRUE) {
    total <- 50
    if (total > length(max_combinations)) {
      total <- length(max_combinations)
    }
  }

  combinations <- sample(max_combinations, total)
  results <- data.frame(
    "variables" = "y", "rmse" = 1,
    "aic" = 1,
    "bic" = 2,
    "r-squared" = 2,
    "significants" = 1,
    "normal_residuals" = "ok"
  )

  for (i in 1:length(combinations)) {
    X <- data[combinations[i] |> unlist()]
    Y <- data[y]

    models <- chen_reg(data.frame(Y, X), Y ~ ., quantile = quantile)

    vars <- paste(X |> names(), collapse = " + ")
    significants <- (\(x) x < alpha)(models$pvalues) |> sum()
    normal_test <- (models$residuals |> shapiro.test())$p.value
    check_residuals <- (ifelse(normal_test > 0.05, "OK", "not-normal"))

    results <- results |> rbind(data.frame(
      "variables" = vars,
      "rmse" = models$metrics$rmse,
      "aic" = models$metrics$aic,
      "bic" = models$metrics$bic,
      "r-squared" = models$metrics$r2,
      "significants" = significants - 1,
      "normal_residuals" = check_residuals
    ))
  }
  results <- results[-1, ]
  results <- results[order(results[metric] |> unlist()), ]
  rownames(results) <- NULL


  if (info == TRUE) {
    cat(paste("The best variables based on", metric, "is", results[1, 1], "\n"))
  }


  return(results)
}
