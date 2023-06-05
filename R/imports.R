#' @importFrom stats model.matrix
#' @importFrom stats  lm.fit
#' @importFrom stats  optim
#' @importFrom stats  qnorm
#' @importFrom stats  pnorm
#' @importFrom stats shapiro.test
#' @importFrom graphics hist
#' @importFrom utils combn
#' @importFrom graphics par
utils::globalVariables(c(
  "V1", "tau", "y", "X", "formula", "coef", "runif",
  "ginv_lig", "quantile", "residc"
))
