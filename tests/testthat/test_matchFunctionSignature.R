context("matchFunctionSignature")

test_that("matchFunctionSignature", {
  expect_false(matchFunctionSignature(NULL))
  expect_true(matchFunctionSignature(function(){}))
  expect_false(matchFunctionSignature(function(x){}))
  expect_true(matchFunctionSignature(sum, function(..., na.rm = FALSE) { NULL }))
  expect_false(matchFunctionSignature(sum, function(..., na.rm) { NULL }))
  expect_false(matchFunctionSignature(function(a, b = a) {}, function(b, a = b) {}))
})
