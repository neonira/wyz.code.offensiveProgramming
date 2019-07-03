context("verifyName")

tn_good <- c('myFunctionName', 'myOtherFunctionName')
tn_bad <- c('MyClassName', 'myParameterName_s_1')
tn_error <- c(56, NA)

test_that("verifyName", {
  myt <- function(aName_s_1) {
    expect_true(verifyName(!!aName_s_1))
  }

  myf <- function(aName_s_1) {
    expect_false(verifyName(!!aName_s_1))
  }

  mye <- function(aName_s_1) {
    expect_error(verifyName(!!aName_s_1))
  }

  sapply(tn_good, myt)
  sapply(tn_bad, myf)
  sapply(tn_error, mye)
})
