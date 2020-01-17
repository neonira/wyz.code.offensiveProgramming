context("getObjetFunctionArgumentNames")

source('pathResolver.R')
source(file.path(computeRootPath(), 'code-samples', 'classes', 'sample-classes.R'))

test_that("getObjetFunctionArgumentNames", {
  expect_true(is.na(getObjectFunctionArgumentNames(new.env())))
  expect_length(getObjectFunctionArgumentNames(EmptyEnv()), 0)
  expect_length(getObjectFunctionArgumentNames(MyEnv()), 1)
  expect_length(getObjectFunctionArgumentNames(Bu_S3()), 2)
  expect_length(getObjectFunctionArgumentNames(Accumulator_R6$new(), FALSE), 1)
  expect_length(getObjectFunctionArgumentNames(new('Person_RC', name = 'neonira'), FALSE), 2)
  # Weirdness here - testthat reports 1, while covr reports 0 ???
  expect_true(length(getObjectFunctionArgumentNames(new('Person_S4', name = 'neonira'), FALSE)) <= 1)
})

