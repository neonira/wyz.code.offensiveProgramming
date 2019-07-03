context("verifyClassName")

tn_bad <- c('myFunctionName', 'myOtherFunctionName', 'myParameterName_s_1')
tn_good <- c('MyClassName', 'MyOtherClassName')
tn_error <- c(56, NA)

test_that("verifyClassName", {
  myt <- function(aName_s_1) {
    expect_true(verifyClassName(!!aName_s_1))
  }

  myf <- function(aName_s_1) {
    expect_false(verifyClassName(!!aName_s_1))
  }

  mye <- function(aName_s_1) {
    expect_error(verifyClassName(!!aName_s_1))
  }

  sapply(tn_good, myt)
  sapply(tn_bad, myf)
  sapply(tn_error, mye)
})
