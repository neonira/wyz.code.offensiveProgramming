context("getObjectClassKind")

test_that("getObjectClassKind", {
  expect_true(is.na(getObjectClassKind(new.env())))
  expect_equal(getObjectClassKind(Bu_S3()), 'S3')
  expect_equal(getObjectClassKind(new('Person_S4', name = 'neonira')), 'S4')
  expect_equal(getObjectClassKind(new('Person_RC', name = 'neonira')), 'RC')
  expect_equal(getObjectClassKind(Accumulator_R6$new()), 'R6')
  expect_equal(getObjectClassKind(MyEnv()), 'environment')
})
