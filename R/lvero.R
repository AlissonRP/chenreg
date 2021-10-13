
lvero <- function(theta, data) {
  n <- nrow(data[, 1])
  lambda <- theta[1]

  beta <- theta[2:length(theta)]

  eta <- X %*% as.matrix(beta)
  md <- ginv_lig(eta)

  lv <- suppressWarnings(log(log(1 - tau) / (1 - exp(md^lambda))) + log(lambda) + (lambda - 1) * log(y) +
    (log(1 - tau) / (1 - exp(md^lambda))) * (1 - exp(y^lambda)) + (y^lambda))
  return(sum(lv))
}
