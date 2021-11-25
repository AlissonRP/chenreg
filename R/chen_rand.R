#' chen_rand
#'
#'  random generation for the chen distribution.
#'
#' @param n  number of observations. If length(n) > 1, the length is taken to be the number required
#'
#' @param  tau   a number that indicates the quantile
#'
#' @param lambda shape parameter
#'
#' @details The  reparameterized chen distribution has density:
#' \deqn{f(y;\lambda,\mu,\tau)=log(1-\tau)/(1-e^{\mu^\lambda}) \lambda y^{\lambda-1}
#' e^[[log(1-\tau)/1-exp(\mu^\lambda)][1-exp(y^\lambda)]+y^\lambda ]}
#'
#' @export
chen_rand <- function(n, md, lambda, tau) {
  u <- runif(n)
  y <- (log(1 - (log(1 - u) / (log(1 - tau) / (1 - exp(md^lambda))))))^(1 / lambda)
  return(y)
}
