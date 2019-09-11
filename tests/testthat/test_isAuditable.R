context("isAuditable")

b <- isAuditable()

if (!b) {
  Sys.setenv("OP_AUDIT" = 1)
  b <- isAuditable()
}

test_that("isAuditable", {
  expect_true(b)
})

Sys.setenv("OP_AUDIT" = '')
