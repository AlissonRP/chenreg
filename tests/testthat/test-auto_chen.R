library(chenreg)
data = simu

test_that("auto_chen works", {
  best = auto_chen("Y", data, metric = "bic")




  expect_equal(best[1,1], "V2 + V3 + V4")
  expect_error(auto_chen("Y", data, metric = "bic", alpha = 2), "The quantile and alpha must be between 0 and 1")
  expect_error(auto_chen("Y", data, metric = "bic", quantile = 2), "The quantile and alpha must be between 0 and 1")
  expect_warning(auto_chen("Y", data, metric = "bic", total = 70), "The total number of models you want to adjust is greater than the possible quantity.
The total value has been adjusted to the maximum possible.")

})
