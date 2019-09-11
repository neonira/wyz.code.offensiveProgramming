context("runTestCase")

source('pathResolver.R')
fp <- file.path(computeRootPath(), 'code-samples/both-defs/good/full/AdditionTCFI_G1.R')
# cat('path', fp, '\n')
source(fp)
tc <- runTestCase(AdditionTCFI_G1(), c(3, 4), EvaluationMode(defineEvaluationModes()[3]))

test_that("runTestCase", {
  expect_length(tc, 2)
  expect_equal(names(tc), c('raw', 'synthesis'))
  expect_true(all(tc$synthesis$status))
})
