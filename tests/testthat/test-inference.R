library(chenreg)
set.seed(42)
modell <- chenreg::chen_reg(Y ~ ., data = simu[, -1], tau = 0.5, link = "log")
modelsq <- chenreg::chen_reg(Y ~ ., data = simu[, -1], tau = 0.9, link = "sqrt")
model <- suppressWarnings(chenreg::chen_reg(Y ~ ., data = simu, tau = 0.1, link = "log"))
test_that("residuals ok", {
  expect_true(-0.2 < mean(modell$residual) & mean(modell$residual) < 0.2)
  expect_true(-0.2 < mean(modelsq$residual) & mean(modelsq$residual) < 0.2)
})

test_that("residuals not ok", {
  expect_false(-0.2 < mean(model$residual) & mean(model$residual) < 0.2)
})

test_that("p-values  ok", {
  expect_true(modell$pvalues |> sum() < 0.1)
  expect_true(modelsq$pvalues |> sum() < 0.1)
})
model$pvalues[is.nan(model$pvalues)] <- NA
test_that("p-values not ok", {
  expect_true(is.na(model$pvalues) |> sum() >= 1)
})
