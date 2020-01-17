context("findFilesInPackage")

f <- findFilesInPackage('sample_classes.R', 'wyz.code.offensiveProgramming')
g <- findFilesInPackage('sample-classes.R', 'wyz.code.offensiveProgramming')
test_that("findFilesInPackage", {
  expect_length(f, 1L)
  expect_length(f[[1]], 0L)
  expect_length(g, 1L)
  expect_true(nchar(g[[1]]) > 0L)
})
