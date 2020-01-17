context("retrieveSupportedObjectInformation")

source('pathResolver.R')
source(file.path(computeRootPath(), 'code-samples', 'classes', 'sample-classes.R'))

test_that("retrieveSupportedObjectInformation", {
  expect_error(retrieveSupportedObjectInformation(NULL))
  expect_error(retrieveSupportedObjectInformation(Unknown()))

  expect_true(is.list(retrieveSupportedObjectInformation(Zorg())))
  expect_true(is.list(retrieveSupportedObjectInformation(Bu_S3())))
  expect_true(is.list(retrieveSupportedObjectInformation(new('Person_S4', name = 'neonira'))))
  expect_true(is.list(retrieveSupportedObjectInformation(new('Person_RC', name = 'neonira'))))
  expect_true(is.list(retrieveSupportedObjectInformation(Accumulator_R6$new())))
})
