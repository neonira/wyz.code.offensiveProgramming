context("verifyFunctionReturnTypesDefinition")

source('pathResolver.R')
sp <- file.path(computeRootPath(), 'code-samples')

getSampleFiles <- function(foldername_s) {
  src_folders <- file.path(sp, foldername_s, c('good', 'bad'))
  list.files(src_folders, '*.R', recursive = TRUE, full.names = TRUE)
}

sf <- getSampleFiles('frt-defs')
.sf <- sapply(sf, source, encoding = "UTF-8")

executeControl <- function(filename_s) {
  g <- grepl('/good/', filename_s, fixed = TRUE)
  f <- grepl('/full/', filename_s, fixed = TRUE)
  # kind <- strBracket(ifelse(g, ifelse(f, 'good full', 'good partial'), 'bad'))
  bn <- basename(filename_s)
  # cat(informativeLine(paste(bn, kind), pre_i_1 = 1))
  rbn <- removeFilenameExtension(bn)
  eval(parse(text = paste0('x <- ', rbn, '()')))

  rv <- verifyFunctionReturnTypesDefinition(x, ifelse(g, ifelse(f, TRUE, FALSE), FALSE))
  # catn('expected is', ifelse(exists('label', envir = x), x$label, 'no error'))
  # catn(ifelse(rv$validity == g, 'correct behavior', 'erroneous behavior'))
  list(status = rv$validity == g, result = rv)
}

rv <- sapply(sf, executeControl, simplify = FALSE, USE.NAMES = FALSE)

bad <- Filter(function(e) e$status == FALSE, rv)
print(bad)

test_that("verifyFunctionReturnTypesDefinition", {

  mtf <- function(k) {
    expect_true(rv[[!!k]]$status)
  }

  sapply(seq_len(length(rv)), mtf)
})

test_that("verifyFunctionReturnTypesDefinition - coverage", {

  # not instumented at all
  expect_false(verifyFunctionReturnTypesDefinition(EmptyEnv(), FALSE)$validity)

  # badly instrumented
  expect_false(verifyFunctionReturnTypesDefinition(Zarg(), TRUE)$validity)

  # partially instrumented - check TRUE ==> full instrumentation
  expect_false(verifyFunctionReturnTypesDefinition(Zorg(), TRUE)$validity)

  # partially instrumented - check FALSE ==> partial instrumentation
  expect_true(verifyFunctionReturnTypesDefinition(Zorg(), FALSE)$validity)
})
