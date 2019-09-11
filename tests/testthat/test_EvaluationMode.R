context("EvaluationMode")

test_that("EvaluationMode", {
  expect_error(EvaluationMode(''))
  expect_output(print(EvaluationMode()))
})
