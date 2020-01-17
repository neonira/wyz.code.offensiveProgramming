context("identifyOPInstrumentationLevel")

source('pathResolver.R')
source(file.path(computeRootPath(), 'code-samples', 'classes', 'sample-classes.R'))

objs <- list(Zorg(), Zirg(), Zarg(), Zurg(), MyEnv(), PureR(), EmptyEnv())
rv <- lapply(objs, identifyOPInstrumentationLevel)

getIndex <- function(index_s_1) {
  sapply(rv, function(e) e[[index_s_1]])
}

test_that("identifyOPInstrumentationLevel", {
  expect_equal(getIndex('offensive_programming'), c(rep(TRUE, 4), rep(FALSE, 3)))
  expect_equal(getIndex('full_instrumentation'), rep(FALSE, length(objs)))
  expect_equal(getIndex('semantic_naming'), c(rep(TRUE, 5), rep(FALSE, 2)))
  expect_equal(getIndex('function_return_type'), c(TRUE, FALSE, TRUE, rep(FALSE, 4)))
  expect_equal(getIndex('test_case_definition'), c(FALSE, TRUE, FALSE, TRUE, rep(FALSE, 3)))
})

test_that("identifyOPInstrumentationLevel - coverage", {
  expect_true(is.list(identifyOPInstrumentationLevel(NULL, 'append')))
  expect_true(is.list(identifyOPInstrumentationLevel(NULL, 'Sys.time')))
})
