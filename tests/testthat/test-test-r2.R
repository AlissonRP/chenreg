library(chenReg)
# for (i in c('log','sqrt')){
# r2=NULL
# model=chenReg::chen_reg.fit(Y ~.,data=simu[,-1],tau=0.5, link=i)
# r2[[i]]=model$metrics$r2
# }
set.seed(42)
modell <- chenReg::chen_reg.fit(Y ~ ., data = simu[, -1], tau = 0.5, link = "log")
modelsq <- chenReg::chen_reg.fit(Y ~ ., data = simu[, -1], tau = 0.8, link = "sqrt")
test_that("r2", {
  expect_equal(round(modell$metrics$r2, 2), .98)
  expect_equal(round(modelsq$metrics$r2, 2), .6)
})
