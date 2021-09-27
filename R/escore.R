escore <- function(theta, data) {
  lambda <- theta[1]
  beta <- theta[2:length(theta)]
  y <- data[, 1] |>
    unlist()
  X <- model.matrix(formula, data)

  eta <- as.vector(X %*% as.matrix(beta))
  md <- ginv_lig(eta)
  mB <- as.vector(-(lambda * md^(lambda - 1) * exp(md^lambda) * (exp(md^lambda) +
    log(1 - tau) * exp(y^lambda) - log(1 - tau) - 1)) / ((exp(md^lambda) - 1)^2))
  mL <- as.vector(((-log(1 - tau) * y^lambda * log(y) * exp(y^lambda) + (md^lambda) * log(md) * exp(md^lambda)) / (1 - exp(md^lambda)))
  + ((log(1 - tau) * (md^lambda) * log(md) * exp(md^lambda) * (1 - exp(y^lambda))) / ((1 - exp(md^lambda))^2)) +
    1 / lambda + y^lambda * log(y) + log(y))


  mT <- diag(exp(eta))

  Ulambda <- sum(mL)
  Ubeta <- t(X) %*% mT %*% mB

  rval <- c(Ulambda, Ubeta)
  return(rval)
}
