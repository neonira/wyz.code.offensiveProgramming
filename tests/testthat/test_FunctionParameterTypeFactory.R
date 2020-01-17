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
  expect_true(grepl('A length-1 or 3 vector of string values', ptf$getTypeDescription(FunctionParameterName('x_s_3n')), fixed = TRUE))
  expect_true(grepl('A length-3 or less vector of string values', ptf$getTypeDescription(FunctionParameterName('x_s_3l')), fixed = TRUE))
  expect_true(grepl('A length-3 or more vector of string values', ptf$getTypeDescription(FunctionParameterName('x_s_3m')), fixed = TRUE))
  expect_false(ptf$verifyValue(FunctionParameterName('x_d_1'), 1:7)$validity)
  expect_true(ptf$verifyValue(FunctionParameterName('x_'), 1:7)$validity)
})

test_that("FunctionParameterTypeFactory - coverage", {

  expect_true(ptf$isPureBoolean(TRUE))
  expect_true(ptf$isPureBoolean(FALSE))
  expect_false(ptf$isPureBoolean(1L))
  expect_false(ptf$isPureBoolean(NA))
  expect_true(ptf$isPureBoolean(logical(0)))

  expect_true(ptf$isPureComplex(complex(real = 3L, imaginary = -7)))
  expect_false(ptf$isPureComplex(-4L))
  expect_true(ptf$isPureComplex(vector('complex', 0)))
  expect_false(ptf$isPureComplex(complex(real = NA_integer_, imaginary = 1L)))
  expect_false(ptf$isPureComplex(complex(real = Inf, imaginary = 1L)))

  expect_false(ptf$isPureInteger(TRUE))
  expect_true(ptf$isPureInteger(-1L))
  expect_true(ptf$isPureInteger(integer(0)))
  expect_false(ptf$isPureInteger(double()))
  expect_true(ptf$isPureInteger(1:3))
  expect_false(ptf$isPureInteger(1:3 * 1.0))

  expect_true(ptf$isPureMathInteger(-1L))
  expect_true(ptf$isPureMathInteger(integer(0)))
  expect_false(ptf$isPureMathInteger(double(0)))
  expect_true(ptf$isPureMathInteger(1:3))
  expect_false(ptf$isPureMathInteger(NA_integer_))
  expect_false(ptf$isPureMathInteger(Inf))
  expect_false(ptf$isPureMathInteger(-Inf))

  expect_false(ptf$isPureReal(-1L))
  expect_false(ptf$isPureReal(integer(0)))
  expect_true(ptf$isPureReal(double(0)))
  expect_true(ptf$isPureReal(3.4))
  expect_false(ptf$isPureReal(1:3))
  expect_false(ptf$isPureReal(NA_integer_))
  expect_false(ptf$isPureReal(NA_real_))
  expect_false(ptf$isPureReal(Inf))
  expect_false(ptf$isPureReal(-Inf))

  expect_true(ptf$isString('neonira'))
  expect_true(ptf$isString(character(0)))
  expect_false(ptf$isString(NA_character_))

  expect_false(ptf$addSuffix('zo', 'zorg', function(lam) { lam }))
})


