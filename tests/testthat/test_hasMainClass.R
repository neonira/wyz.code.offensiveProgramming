context("hasMainClass")

test_that("hasMainClass", {
  expect_true(is.na(hasMainClass(new.env(), '')))
  expect_true(hasMainClass(Bu_S3(), 'Bu_S3'))
  expect_true(hasMainClass(new('Person_S4', name = 'neonira'), 'Person_S4'))
  expect_true(hasMainClass(new('Person_RC', name = 'neonira'), 'Person_RC'))
  expect_true(hasMainClass(Accumulator_R6$new(), 'Accumulator_R6'))
  expect_true(hasMainClass(MyEnv(), 'MyEnv'))
  expect_true(hasMainClass(EmptyEnv(), 'EmptyEnv'))
})
