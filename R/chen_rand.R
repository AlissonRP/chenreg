#' chen_rand
#' Generates a random vector using a Chen distribution. Thus,
#' the `chen_norm()` function simulates random variables having a specified Chen Distribution.
#'
#'
#' @param n  number of observations. If length(n) > 1, the length is taken to be the number required
#'
#' @param  tau    indicates the quantile
#'
#' @param lambda shape parameter
#' @param md median
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
#' rand <- chenReg::chen_rand(n, md, lambda, tau = 0.7)
#' @note
#' Default of lambda is 0.1
#'
#' Default of tau is 0.8
#' @export
chen_rand <- function(n = 1, md, lambda = 0.1, tau = 0.8) {
  u <- runif(n)
  y <- (log(1 - (log(1 - u) / (log(1 - tau) / (1 - exp(md^lambda))))))^(1 / lambda)
  return(y)
}
