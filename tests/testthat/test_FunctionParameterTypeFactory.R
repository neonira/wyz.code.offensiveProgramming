context("FunctionParameterTypeFactory")

ptf <- retrieveFactory()

test_that("FunctionParameterTypeFactory", {
  expect_true(is.na(ptf$getRowNumber('xxx')))
  expect_true(is.integer(ptf$getRowNumber('s'))) # suffix
  expect_true(is.integer(ptf$getRowNumber('boolean'))) # type
  expect_false(ptf$addSuffix('xxx', 'xxx', 'xxx')) # 3rd must be a function

  expect_true(startsWith(ptf$getVerificationFunction('xxx'), "No verification function"))
  expect_true(is.function(ptf$getVerificationFunction('boolean')))

  expect_true(startsWith(ptf$getType('xxx'), 'No suffix or type'))
  expect_equal(ptf$getType('i'), 'integer')

  expect_true(grepl('double', ptf$getTypeDescription(FunctionParameterName('x_d')), fixed = TRUE))
  expect_true(grepl('variable type', ptf$getTypeDescription(FunctionParameterName('x_')), fixed = TRUE))
  expect_true(grepl('matrix', ptf$getTypeDescription(FunctionParameterName('x_m')), fixed = TRUE))
  expect_true(grepl('double', ptf$getTypeDescription(FunctionParameterName('x_d_3l')), fixed = TRUE))
  expect_true(grepl('double', ptf$getTypeDescription(FunctionParameterName('x_d_3')), fixed = TRUE))
  expect_true(grepl('additional', ptf$getTypeDescription(FunctionParameterName('...')), fixed = TRUE))
  expect_true(grepl('unknown', ptf$getTypeDescription(FunctionParameterName('x')), fixed = TRUE))
  expect_true(grepl('Vector length must be 1 or 3', ptf$getTypeDescription(FunctionParameterName('x_s_3n')), fixed = TRUE))
  expect_true(grepl('Vector length must be 3 or less', ptf$getTypeDescription(FunctionParameterName('x_s_3l')), fixed = TRUE))
  expect_true(grepl('Vector length must be 3 or more', ptf$getTypeDescription(FunctionParameterName('x_s_3m')), fixed = TRUE))
  expect_false(ptf$verifyValue(FunctionParameterName('x_d_1'), 1:7)$validity)
  expect_true(ptf$verifyValue(FunctionParameterName('x_'), 1:7)$validity)
})
