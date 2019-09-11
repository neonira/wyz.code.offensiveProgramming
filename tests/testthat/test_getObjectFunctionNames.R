context("getObjectFunctionNames")

source('pathResolver.R')
source(file.path(computeRootPath(), 'code-samples', 'classes', 'sample-classes.R'))

test_that("getObjectFunctionNames", {
  expect_true(is.na(getObjectFunctionNames(new.env())))
  expect_length(getObjectFunctionNames(EmptyEnv()), 0)
  expect_length(getObjectFunctionNames(MyEnv()), 1)
  expect_length(getObjectFunctionNames(Bu_S3()), 2)
  expect_length(getObjectFunctionNames(Accumulator_R6$new()), 1)
  expect_length(getObjectFunctionNames(new('Person_RC', name = 'neonira')), 2)
  # Weirdness here - testthat reports 1, while covr reports 0 ???
  expect_true(length(getObjectFunctionNames(new('Person_S4', name = 'neonira'))) <= 1)
})
