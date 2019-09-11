context("FunctionParameterName")

tn_good <-  c('value_d', 'value_d_1', 'value_d_1l', 'value_d_1m', 'value_lo_450',
              'value_d_5', 'value_d_5l', 'value_d_5m', 'value_d_5n', 'value_7n_',
              'value_', 'value_1_', 'value_1l_', 'value_1m_',
              'frenchCharacterText_s', '...', 'object_o',  'object_o_1', 'value1_',
              'value003_')

tn_R_valid <- c('standard_param', 'param_z')

tn_bad <- c('value_d_1n', 'param_b_', 'param_1_1', 'param_1k', '.alpha', '.Beta', '.opt',
            'param_1M', 'aCamelizedParam', 'agréée', 'object_o_1_', 'object_o_',
            'value_1', 'value_1l', 'value_1m', 'value_5n', 'Label_z' )

tn_error <- c(56, NA)

singleCase <- function(parameterName_s_1) {
  tryCatch(list(status = 'standard', value = FunctionParameterName(parameterName_s_1)$isValid(), message = 'normal execution'),
           error = function(e) list(status = 'error', value = e, message = e$message),
           warning = function(w) list(status = 'warning', value = w, message = w$message))$value
}

test_that("FunctionParameterName - any kind of parameter names", {
  myt <- function(aName_s_1) {
    expect_true(singleCase(!!aName_s_1))
  }

  myf <- function(aName_s_1) {
    expect_false(singleCase(!!aName_s_1))
  }

  mye <- function(aName_s_1) {
    expect_error(FunctionParameterName(!!aName_s_1))
  }

  sapply(tn_good, myt)
  sapply(tn_R_valid, myt)
  sapply(tn_bad, myf)
  sapply(tn_error, mye)
})


checkSemanticNames <- function(parameterName_s_1) {
  FunctionParameterName(parameterName_s_1)$isSemanticName()
}

test_that("FunctionParameterName - only semantic names", {
  myt <- function(aName_s_1) {
    expect_true(checkSemanticNames(!!aName_s_1))
  }

  myf <- function(aName_s_1) {
    expect_false(checkSemanticNames(!!aName_s_1))
  }

  sapply(tn_good, myt)
  sapply(tn_R_valid, myf)
})


fpns <- list(
  FunctionParameterName('alpha_d_3n'),
  FunctionParameterName('alpha_d_3l'),
  FunctionParameterName('alpha_d_3m'),
  FunctionParameterName('alpha_d_3')
)

lm <- c('n', 'l', 'm')

test_that("FunctionParameterName - coverage", {

  mtf <- function(k) {
    expect_true(fpns[[!!k]]$isPreValid())
    expect_true(fpns[[!!k]]$isValid())
    expect_true(fpns[[!!k]]$hasCompliantLength(letters[1:3]))
    expect_output(cat(fpns[[!!k]]$toString()))
    if (k < 4) {
      expect_equal(fpns[[!!k]]$getLengthModifier(), lm[k])
    } else {
      expect_true(is.na(fpns[[!!k]]$getLengthModifier()))
    }
  }

  expect_error(FunctionParameterName(c('alpha', 'beta')))
  expect_equal(FunctionParameterName('myIdentifier_')$deduceParameterLabel(), 'my identifier')
  expect_true(is.na(FunctionParameterName('myIdentifier_')$getLengthSpecification()))
  expect_equal(FunctionParameterName('myIdentifier_s_3')$getLengthSpecification(), '3')
  expect_equal(FunctionParameterName('myIdentifier_s_3n')$getLengthSpecification(), '3n')

  sapply(seq_len(length(fpns)), mtf)
})


