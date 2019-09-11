context("getClassTypicalFunctionNames")

source('pathResolver.R')
source(file.path(computeRootPath(), 'code-samples', 'classes', 'sample-classes.R'))

test_that("getClassTypicalFunctionNames", {
  expect_true(is.na(getClassTypicalFunctionNames(new.env())))
  expect_length(getClassTypicalFunctionNames(Bu_S3()), 0)
  expect_length(getClassTypicalFunctionNames(new('Person_S4', name = 'neonira')), 2)
  expect_length(getClassTypicalFunctionNames(new('Person_RC', name = 'neonira')), 12)
  expect_length(getClassTypicalFunctionNames(Accumulator_R6$new()), 1)
  expect_length(getClassTypicalFunctionNames(MyEnv()), 0)
})

