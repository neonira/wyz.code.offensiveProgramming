context("runTestCase")

source('pathResolver.R')
source(file.path(computeRootPath(), 'code-samples/full-instrumentation/AdditionTCFI_G1.R'))
tc <- runTestCase(AdditionTCFI_G1(), c(3, 4), EvaluationMode(defineEvaluationModes()[3]))

test_that("runTestCase", {
  expect_length(tc, 2)
  expect_equal(names(tc), c('raw', 'synthesis'))
  expect_true(all(tc$synthesis$status))
})
