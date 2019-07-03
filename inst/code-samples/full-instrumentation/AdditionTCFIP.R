AdditionTCFIP <- function() {
  self <- environment()
  class(self) <- append('AdditionTCFIP', class(self))

  addNumeric <- function(x_n, y_n) x_n + y_n

  addDouble <- function(x_d, y_d = 0.0, ...) x_d + y_d + ...

  addInteger <- function(x_i, y_i) x_i + y_i

  divideByZero <- function(x_n) x_n / 0

  generateWarning <- function(x_ = 8L) 1:3 + 1:7 + x_

  generateError <- function() stop('generated error')

  function_return_types <- data.table(
    function_name = c('addNumeric', 'addDouble', 'addInteger',
                      'divideByZero', 'generateWarning', 'generateError'),
    return_value = c('x_n', 'x_d', 'x_i','x_d', 'x_w', 'x_er')
  )

  test_case_definitions <- data.table(
    function_name = c('addDouble', 'addInteger', 'divideByZero', "divideByZero", 'generateWarning', 'generateError'),
    standard_evaluation = c('correct', 'correct', 'correct', 'correct', 'correct', 'failure'),
    type_checking_enforcement = c('correct', 'erroneous', 'correct', 'correct', 'correct', 'correct'),
    test_case = list(
      #addDouble
      TestCaseDefinition(list(as.double(34L), 44.5), 78.5, 'sum 2 doubles'),

      # addInteger - se => erroneous, tci => failure
      TestCaseDefinition(list(34L, 44.5), 78L, 'sum 1 integer and 1 double'),

      # divideByZero - correct
      TestCaseDefinition(list(1), Inf, '1 / 0'),
      TestCaseDefinition(list(0), NaN, '0 / 0'),

      # generateWarning
      TestCaseDefinition(list(0), 1:3 + 1:7, 'generate warning'),

      # generateError
      TestCaseDefinition(list(), NA, 'generate error')
    )
  )

  self
}
