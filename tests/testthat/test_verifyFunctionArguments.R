context("verifyFunctionArguments")

myFunction <- function(list_l, abort_b_1 = FALSE, verbosity_b_1 = FALSE) {
  verifyFunctionArguments(mget(ls()),
                          abort_b_1 = abort_b_1,
                          verbosity_b_1 = verbosity_b_1)
}

myf <- function() {
  verifyFunctionArguments(mget(ls()))
}

test_that("verifyFunctionArguments", {
  expect_true(myf()) # no argument case
  # expect_false(myf(3L)) # error unused argument (3) triggered by R directly
  expect_true(myFunction(list(1:3)))
  expect_false(myFunction(1:3))
  expect_output(myFunction(list(1:3), FALSE, TRUE))
  expect_error(myFunction(1:3, TRUE))
})
