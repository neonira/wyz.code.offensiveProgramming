AdditionTC_ <- function() {
  self <- environment()
  class(self) <- append('AdditionTC_', class(self))

  addDouble <- function(x_d, y_d) x_d + y_d

  addInteger <- function(x_i, y_i) x_i + y_i

  divideByZero <- function(x_n) x_n / 0

  generateWarning <- function(x_) 1:3 + 1:7

  generateError <- function() stop('generated error')

  test_case_definitions <- data.table(
    function_name = c(rep('addDouble', 9), rep('addInteger', 9), rep('divideByZero', 3), 'generateWarning', 'generateError'),
    standard_evaluation = c(rep('correct', 5), 'erroneous', rep('correct', 7), 'erroneous', rep('correct', 8), 'failure'),
    type_checking_enforcement = c(rep('correct', 5), 'erroneous', rep('failure', 3), rep('correct', 4), rep('failure', 5),
                                  rep('correct', 3), 'correct', 'erroneous'), # correct for generateWarning if options(warn = x < 2)
    test_case = list(
      #addDouble
      TestCaseDefinition(list(as.double(34L), 44.5), 78.5, 'sum 2 doubles'),
      TestCaseDefinition(list(34.0, NA_real_), NA_real_, 'sum 1 double and 1 NA_real_'),
      TestCaseDefinition(list(NA_real_, NA_real_), NA_real_, 'sum 2 NA_real_'),
      TestCaseDefinition(list(NaN, NaN), NaN, 'sum 2 NAN'),
      TestCaseDefinition(list(Inf, Inf), Inf, 'sum 2 Inf'),

      #addDouble - se => erroneous
      TestCaseDefinition(list(as.integer(34.7), as.integer(44.9)), 80, 'sum 2 as.integers confused with sum of rounded value as expectation'),

      #addDouble
      TestCaseDefinition(list(34L, 44.5), 78.5, 'sum of 1 integer and 1 double'),
      TestCaseDefinition(list(34.0, NA_integer_), NA_real_, 'sum of 1 integer and 1 double'),
      TestCaseDefinition(list(NA, NA), NA, 'sum 2 NA'),

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

      # divideByZero - correct
      TestCaseDefinition(list(1), Inf, '1 / 0'),
      TestCaseDefinition(list(-1), -Inf, '-1 / 0'),
      TestCaseDefinition(list(0), NaN, '0 / 0'),

      # generateWarning
      TestCaseDefinition(list(0), 1:3 + 1:7, 'generate warning'),

      # generateError
      TestCaseDefinition(list(), NA, 'generate error')
    )
  )

  self
}
