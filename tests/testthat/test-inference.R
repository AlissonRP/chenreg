library(chenReg)
modell=chenReg::chen_reg.fit(Y ~.,data=simu[,-1],tau=0.5, link="log")
modelsq=chenReg::chen_reg.fit(Y ~.,data=simu[,-1],tau=0.9, link='sqrt')
model=chenReg::chen_reg.fit(Y ~.,data=simu,tau=0.1, link="log")
test_that('residuals ok',{
  expect_true(-0.2<mean(modell$residual) & mean(modell$residual)<0.2)
  expect_true(-0.2<mean(modelsq$residual) & mean(modelsq$residual)<0.2)
})

test_that('residuals not ok',
          {expect_false(-0.2<mean(model$residual) & mean(model$residual)<0.2)
})
