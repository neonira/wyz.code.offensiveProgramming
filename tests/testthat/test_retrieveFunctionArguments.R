context("retrieveFunctionArguments")

o <- list(
  retrieveFunctionArguments(cos),
  retrieveFunctionArgumentNames(cos),

  retrieveFunctionArguments(append),
  retrieveFunctionArgumentNames(append),

  retrieveFunctionArguments(`$`),
  retrieveFunctionArgumentNames(`$`),
  retrieveFunctionArgumentNames(sum)
)

test_that("retrieveFunctionArguments", {
  expect_equal(length(o[[1]]), length(o[[2]]))
  expect_equal(length(o[[3]]), length(o[[4]]))
  expect_true(is.null(o[[5]]))
})
