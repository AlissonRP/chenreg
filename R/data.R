#' Example Dataset to fit the regression

#' Contains five variables
#' @format  A dataframe with 300 rows and 4 variables
#' The data was generated as follows:
#'
#'
#' @examples
#' # how to generate the data
#' n <- 500
#' lambda <- 1
#' beta <- c(2.5, -2, 3)
#' X <- cbind(runif(n), runif(n), runif(n))
#' eta <- X %*% as.matrix(beta)
#' md <- exp(eta)
#' simu <- chenreg::chen_rand(n, md, lambda, tau = 0.7)
#'
#' @source using the function from package
"simu"
