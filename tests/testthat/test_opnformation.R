context("opInformation")

test_that("opInformation", {
  expect_true('data.table' %in% class(opInformation()))
})
