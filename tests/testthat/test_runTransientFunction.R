context("runTransientFunction")

r1g <- runTransientFunction(cos, list(pi), EvaluationMode(defineEvaluationModes()[1]), 'x_s')
r2w <- runTransientFunction(cos, list(pi), EvaluationMode(defineEvaluationModes()[2]), 'x_s')
r2g <- runTransientFunction(cos, list(pi), EvaluationMode(defineEvaluationModes()[2]), 'x_d')

r3w <- runTransientFunction(cos, list(pi), EvaluationMode(defineEvaluationModes()[3]), 'x_d')

opcos <- function(angleInRadian_d) cos(angleInRadian_d)

r3g <- runTransientFunction(opcos, list(pi), EvaluationMode(defineEvaluationModes()[3]), 'x_d')

test_that("runTransientFunction", {
  expect_error(runTransientFunction(NA, list(), EvaluationMode(defineEvaluationModes()[1]), 'x_s'))

  expect_error(runTransientFunction(cos, c(), EvaluationMode(defineEvaluationModes()[1]), 'x_s'))

  expect_true(r1g$status)
  expect_equal(r1g$value, -1)
  expect_equal(r1g$mode, defineEvaluationModes()[1])

  expect_false(r2w$status)
  expect_equal(r2w$value, -1)
  expect_equal(r2w$mode, defineEvaluationModes()[2])

  expect_true(r2g$status)
  expect_equal(r2g$value, -1)
  expect_equal(r2g$mode, defineEvaluationModes()[2])

  expect_false(r3w$status)
  expect_equal(r3w$value, -1)
  expect_equal(r3w$mode, defineEvaluationModes()[3])

  expect_true(r3g$status)
  expect_equal(r3g$value, -1)
  expect_equal(r3g$mode, defineEvaluationModes()[3])

})
