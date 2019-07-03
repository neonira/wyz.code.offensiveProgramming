context("defineFunctionReturnTypesParameterName")

test_that("defineFunctionReturnTypesParameterName", {
  expect_length(defineFunctionReturnTypesParameterName(), 1)
  expect_equal(defineFunctionReturnTypesParameterName(), 'function_return_types')
})
