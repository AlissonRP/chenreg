library(chenreg)
data <- simu[, -1]
best = auto_chen("Y", data, metric = "bic", info = T)



test_that("auto_chen works", {
  expect_equal(best[1,1], "V2 + V3 + V4")
})
