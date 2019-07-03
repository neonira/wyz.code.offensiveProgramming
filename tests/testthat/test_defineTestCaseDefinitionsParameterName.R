context("defineTestCaseDefinitionsParameterName")

test_that("defineTestCaseDefinitionsParameterName", {
  expect_length(defineTestCaseDefinitionsParameterName(), 1)
  expect_equal(defineTestCaseDefinitionsParameterName(), 'test_case_definitions')
})
