library(chenReg)
set.seed(42)
rand_chen1 <- chenReg::chen_rand(500, 0.5, 1, 0.9)
rand_chen2 <- chenReg::chen_rand(500, 0.2, 0.5, 0.2)
test_that("assymetric", {
  expect_true(mean(rand_chen1) > median(rand_chen1))
  expect_true(mean(rand_chen2) > median(rand_chen2))
})
