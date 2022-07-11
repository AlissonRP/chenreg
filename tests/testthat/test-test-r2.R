library(chenReg)

set.seed(42)
modell <- chenReg::chen_reg(Y ~ ., data = simu[, -1], quantile = 0.5, link = "log")
modelsq <- chenReg::chen_reg(Y ~ ., data = simu[, -1], quantile = 0.8, link = "sqrt")
test_that("r2", {
  expect_equal(round(modell$metrics$r2, 2), .98)
  expect_equal(round(modelsq$metrics$r2, 2), .6)
})
