Addition_TCFI_Partial_S3 <- function() {
  value <- list()
  attr(value, 'class') <- 'Addition_TCFI_Partial_S3'

  # only 4 functions are recorded as generic, 2 are declared here
  # ==> partial verifications will be ok, full verifications will be ko
  value$function_return_types <- data.table(
    function_name = c('addNumeric',  'generateError'),
    return_value = c('x_n', 'x_er')
  )

  value$test_case_definitions <- data.table(
    function_name = c(rep('addInteger', 9), 'generateError'),
    standard_evaluation = c(rep('correct', 4), 'erroneous', rep('correct', 4), 'failure'),
    type_checking_enforcement = c(rep('correct', 4), rep('failure', 5), 'failure'),
    test_case = list(
      #addInteger - correct
      TestCaseDefinition(list(34L, as.integer(44.5)), 78L, 'sum 2 integers'),
      TestCaseDefinition(list(34L, NA_integer_), NA_integer_, 'sum 1 integer and 1 NA_integer'),
      TestCaseDefinition(list(NA_integer_, NA_integer_), NA_integer_, 'sum 2 NA_integer'),
      TestCaseDefinition(list(as.integer("45.654"), 44L), 89L, 'sum a converted string with one integer'),

      # addInteger - se => erroneous, tci => failure
      TestCaseDefinition(list(34L, 44.5), 78L, 'sum 1 integer and 1 double'),

      # addInteger - se => correct, tci => failure
      TestCaseDefinition(list(34L, Inf), Inf, 'sum 1 integer and 1 Inf'),
      TestCaseDefinition(list(34L, NaN), NaN, 'sum 1 integer and 1 NAN'),
      TestCaseDefinition(list(34L, NA), NA, 'sum 1 integer and 1 NA'),
      TestCaseDefinition(list(c(34L, 35L), 44L), c(78L, 79L), 'sum a vector of 2 integers with 1 integer'),

      # generateError
      TestCaseDefinition(list(), NA, 'generate error')
    )
  )

  value$label <- 'partial instrumentation of S3 - Should work with type checking enforcement'

  value
}

addNumeric <- function(object_o_1, x_n, y_n) UseMethod('addNumeric', object_o_1)

addNumeric.Addition_TCFI_Partial_S3 <- function(object_o_1, x_n, y_n) x_n + y_n

addDouble.Addition_TCFI_Partial_S3 <- function(object_o_1, x_d, y_d) x_d + y_d

addInteger <- function(object_o_1, x_i, y_i) UseMethod('addInteger', object_o_1)

addInteger.Addition_TCFI_Partial_S3 <- function(object_o_1, x_i, y_i) x_i + y_i

divideByZero.Addition_TCFI_Partial_S3 <- function(object_o_1, x_n) x_n / 0

generateWarning.Addition_TCFI_Partial_S3 <- function(object_o_1) 1:3 + 1:7

generateWarning2.Addition_TCFI_Partial_S3 <- function(object_o_1) 1:3 + 1:7

generateWarning2 <- function(object_o_1, x_n, y_n) UseMethod('generateWarning2', object_o_1)

generateError <- function(object_o_1) UseMethod('generateError', object_o_1)
generateError.Addition_TCFI_Partial_S3 <- function(object_o_1) stop('generated error')


# a <- Addition_TCFI_Partial_S3()
# print(addNumeric(a, 3, 4.1))
# print(addNumeric.Addition_TCFI_Partial_S3(a, 3, 4.1))
# #print(a.addNumeric(3, 4.1))
# methods(class = 'Addition_TCFI_Partial_S3')
# methods('addNumeric')
#
# print(addDouble.Addition_TCFI_Partial_S3(a, 3, 4.1))

# Reminder
# Calling a method directly is possible, generic or not. Use <methodName>.<className>(object, arguments, ...)
# Calling a generic method is possible. Use <methodName>(object, arguments, ...)
#
# Code transformation
# Need an extraneous argument as first argument to function call, that is the object to consider
#