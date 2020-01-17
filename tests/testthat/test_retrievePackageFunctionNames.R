context("retrievePackageExportedFunctionNames")

pn <- c('base', 'utils', 'crayon', 'wyz.code.offensiveProgramming')

r <- lapply(pn, function(e) {
  guardExecution(retrievePackageFunctionNames(e))
})

test_that("retrievePackageExportedFunctionNames", {

  expect_error(retrievePackageFunctionNames('translation'))

  mtf <- function(k) {
      expect_true(length(r[[!!k]]) > 0)
  }

  sapply(seq_len(length(pn)), mtf)
})
