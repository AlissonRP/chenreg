library(chenreg)

set.seed(42)
test_that("r2", {
  modell <- chenreg::chen_reg(Y ~ ., data = simu[, -1], tau = 0.5, link = "log")
  modelsq <- suppressWarnings(chenreg::chen_reg(Y ~ ., data = simu[, -1], tau = 0.8, link = "sqrt"))
  expect_true(round(modell$metrics$r2, 2) > .95)
  expect_true(round(modelsq$metrics$r2, 2) < .8)
})
