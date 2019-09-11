context("packageFunctionsInformation")

test_that("packageFunctionsInformation", {
  expect_true('data.table' %in% class(packageFunctionsInformation()))
})
