context("TestCaseDefinition")

t <- TestCaseDefinition(list(as.double(34L), 44.5), 78.5, 'a test case definition')

test_that("TestCaseDefinition - for coverage", {
  expect_output(print(t))
})
