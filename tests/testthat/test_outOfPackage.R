context("outOfPackage")

o <- list(
  Accumulator_R6$new(),
  new('Person_S4', name = 'neonira'),
  new('Person_RC', name = 'neonira')
)

o[[3]]$setName('Fabien')
o[[3]]$setAge(as.integer(runif(1, 25, 30)))

test_that("outOfPackage - coverage", {
  expect_equal(guardExecution( 1, FALSE), 1)
  expect_equal(o[[1]]$add(1), 1)
  expect_output(show(o[[2]]))
  expect_equal(name(o[[2]]), 'neonira')
  expect_equal(o[[3]]$name, 'Fabien')
})
