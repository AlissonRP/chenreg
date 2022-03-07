pchen <- function(x, b, lambda) {
  1 - exp(lambda - lambda * exp(x^b))
}
