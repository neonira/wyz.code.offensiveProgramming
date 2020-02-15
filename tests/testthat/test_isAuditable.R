context("isAuditable")

test_that("isAuditable", {
  options('op_audit' =  FALSE)
  expect_false(isAuditable())
  options('op_audit' =  TRUE)
  expect_true(isAuditable())
  options('op_audit' =  NULL)
  expect_false(isAuditable())
})
