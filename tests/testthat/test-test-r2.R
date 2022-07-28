library(chenReg)

set.seed(42)
test_that("r2", {
  modell <- chenReg::chen_reg(Y ~ ., data = simu[, -1], quantile = 0.5, link = "log")
  modelsq <- suppressWarnings(chenReg::chen_reg(Y ~ ., data = simu[, -1], quantile = 0.8, link = "sqrt"))
  expect_true(round(modell$metrics$r2, 2) > .95)
  expect_true(round(modelsq$metrics$r2, 2) <  .8)
})
