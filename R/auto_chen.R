
#' auto_chen
#'
#' auto_chen is used to find the best combinations of exogenous variables based
#' on the metrics that you interested

#'
#' @param y 	is the name of the target variable
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
#' @param info if is TRUE will print in console more information
#'
#' @examples
#' library(chenreg)
#' data <- simu[, -1]
#'
#' auto_chen("Y", data, metric = "rmse", total = 3)
#' auto_chen("Y", data, metric = "bic", info = TRUE)
#'
#'
#' @note
#' the default value of `total` is 50, but if 50
#' is more than the possible number of combinations `(k)`,
#' then `k` will be used as the total.







#' @export
auto_chen <- function(y, data, metric = "rmse", quantile = 0.5, alpha = 0.05, total = NULL, info = FALSE) {

  if ((((0 < quantile) && (quantile < 1)) && ((0 < alpha) && (alpha < 1))) == FALSE){
    stop("The quantile and alpha must be between 0 and 1")
  }

  x_names <- data[, !(names(data) %in% y)] |> names()




  metric <- tolower(metric)

  max_combinations <- do.call("c", lapply(seq_along(x_names), \(i) combn(x_names, i, FUN = list)))


  if (is.null(total) == TRUE) {
    total <- 50
    if (total > length(max_combinations)) {
      total <- length(max_combinations)
    }
  }


  if (total > length(max_combinations)) {
    warning("The total number of models you want to adjust is greater than the possible quantity.
The total value has been adjusted to the maximum possible.")
    total <- length(max_combinations)
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
  n = 0
  count_errors = list()

  for (i in 1:length(combinations)) {
    X <- data[combinations[i] |> unlist()]
    Y <- data[y]
    tryCatch(
      expr = {

    vars <- paste(X |> names(), collapse = " + ")
    models <- chen_reg(data.frame(Y, X), Y ~ ., quantile = quantile)

    significants <- (\(x) x < alpha)(models$pvalues) |> sum()
    normal_test <- (models$residuals |> shapiro.test())$p.value
    check_residuals <- (ifelse(normal_test > alpha, "OK", "not-normal"))


    results <- results |> rbind(data.frame(
      "variables" = vars,
      "rmse" = models$metrics$rmse,
      "aic" = models$metrics$aic,
      "bic" = models$metrics$bic,
      "r-squared" = models$metrics$r2,
      "significants" = significants - 1,
      "normal_residuals" = check_residuals
    ))
      },

    error = function(e){
      count_errors <<- c(count_errors, list(e))

      if (info == TRUE){
      message(paste("the model with ", vars ," not fitted because of an error"))


      print(count_errors)
      }

    }


)
  n = n + 1
  if (n == length(combinations)){
    message(paste('All done. ', 'Out of a total of', total,",", length(count_errors), 'models were not adjusted due to error'))
  }

  }
  results <- results[-1, ]
  results <- results[order(results[metric] |> unlist()), ]
  rownames(results) <- NULL


  if (info == TRUE) {
    message(paste("The best variables based on", metric, "is", results[1, 1], "\n"))
  }


  return(results)
}
