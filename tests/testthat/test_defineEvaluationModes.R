context("defineEvaluationModes")

test_that("defineEvaluationModes", {
  expect_length(defineEvaluationModes(), 3)
  expect_equal(defineEvaluationModes()[1], 'standard_R_evaluation')
  expect_equal(defineEvaluationModes()[2], 'enhanced_R_evaluation')
  expect_equal(defineEvaluationModes()[3], 'type_checking_enforcement')
})
