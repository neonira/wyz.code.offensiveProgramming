context("retrieveFunctionArguments")

o <- list(
  retrieveFunctionArguments(cos),
  retrieveFunctionArguments(append),
  retrieveFunctionArgumentNames(cos),
  retrieveFunctionArgumentNames(append)
)

test_that("retrieveFunctionArguments", {
  expect_equal(length(o[[1]]), length(o[[3]]))
  expect_equal(length(o[[2]]), length(o[[4]]))
})
