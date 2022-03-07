
#'
lvero2 <- function(param) {
  lambda <- param[1]
  md <- param[2]
  lv2 <- suppressWarnings(log(log(1 - tau) / (1 - exp(md^lambda))) + log(lambda) + (lambda - 1) * log(y) +
    (log(1 - tau) / (1 - exp(md^lambda))) * (1 - exp(y^lambda)) + (y^lambda))
  sum(lv2)
}
