#' rchen
#'
#' Generates a random vector using the Chen distribution. Thus,
#' the `chen_rand()` function simulates random variables having a specified Chen
#'  Distribution.
#'
#'
#' @param n  number of observations. If length(n) > 1, the length is taken to be the number required
#'
#' @param  tau    indicates the quantile
#'
#' @param lambda shape parameter
#' @param quantile median
#'
#' @details The  reparameterized chen distribution has density:
#' \deqn{f(y;\lambda,\mu,\tau)=log(1-\tau)/(1-e^{\mu^\lambda}) \lambda y^{\lambda-1}
#' e^[[log(1-\tau)/1-exp(\mu^\lambda)][1-exp(y^\lambda)]+y^\lambda ]}
#'
#' @examples
#' n <- 500
#' lambda <- 1
#' beta <- c(2.5, -2, 3)
#' X <- cbind(runif(n), runif(n), runif(n))
#' eta <- X %*% as.matrix(beta)
#' md <- exp(eta)
#' rand <- chenreg::rchen(n, md, lambda, tau = 0.7)
#' @note
#' Default of lambda is 0.1
#'
#' Default of tau is 0.8
#' @return A vector of size `n`
#'
#' @export
rchen <- function(n = 1, quantile, lambda = 0.1, tau = 0.8) {
  if (any(c(quantile, lambda, tau, n) < 0)) {
    stop("All of the arguments must be positive")
  }
  u <- runif(n)
  y <- (log(1 - (log(1 - u) / (log(1 - tau) / (1 - exp(quantile^lambda))))))^(1 / lambda)
  return(y)
}
