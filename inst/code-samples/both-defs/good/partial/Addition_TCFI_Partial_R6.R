Addition_TCFI_Partial_R6 <- R6::R6Class("Addition_TCFI_Partial_R6", list(
  sum = 0,

  addInteger = function(x_i, y_i) { x_i + y_i },

  generateError = function() { stop('generated error')},

  generateWarning = function() 1:3 + 1:7,

  function_return_types = data.table(
    function_name = c('addInteger', 'generateError'),
    return_value = c('x_n', 'x_er')
  ),

  test_case_definitions = data.table(
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
  ),

  label = 'R6 partial instrumentation'
))
