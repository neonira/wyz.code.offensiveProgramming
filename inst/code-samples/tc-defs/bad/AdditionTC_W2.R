AdditionTC_W2 <- function() {
  self <- environment()
  class(self) <- append('AdditionTC_W2', class(self))

  addNumeric <- function(x_n, y_n) x_n + y_n

  addDouble <- function(x_d, y_d) x_d + y_d

  addInteger <- function(x_i, y_i) x_i + y_i

  addInteger1 <- function(x_i_1, y_i_1) x_i_1 + y_i_1

  addMultiInteger <- function(x_i, y_i, z_i = 0, k_i = 0, ...) {
    x_i + y_i + z_i + k_i + sum(..., na.rm = TRUE)
  }

  test_case_definitions <- list(
    addDouble = list(
      correct = list(
        TestCaseDefinition(list(as.double(34L), 44.5), 78.5, 'sum 2 doubles'),
        TestCaseDefinition(list(34.0, NA_real_), NA_real_, 'sum 1 double and 1 NA_real_'),
        TestCaseDefinition(list(NA_real_, NA_real_), NA_real_, 'sum 2 NA_real_'),
        TestCaseDefinition(list(NaN, NaN), NA_real_, 'sum 2 NAN'),
        TestCaseDefinition(list(Inf, Inf), Inf, 'sum 2 Inf')
      ),

      erroneous = list(
        TestCaseDefinition(list(as.integer(34.7), as.integer(44.9)), 80, 'sum 2 as.integers confused with sum of rounded value as expectation')
      ),

      failure = list(
        TestCaseDefinition(list(34L, 44.5), 78.5, 'sum of 1 integer and 1 double'),
        TestCaseDefinition(list(34.0, NA_integer_), NA_real_, 'sum of 1 integer and 1 double'),
        TestCaseDefinition(list(NA, NA), NA, 'sum 2 NA')
      )
    ),

    addInteger1 = list(
      correct = list(
        TestCaseDefinition(list(34L, as.integer(44.5)), 78L, 'sum 2 integers'),
        TestCaseDefinition(list(34L, NA_integer_), NA_integer_, 'sum 1 integer and 1 NA_integer'),
        TestCaseDefinition(list(NA_integer_, NA_integer_), NA_integer_, 'sum 2 NA_integer')
      ),

      # erroneous = list(
      #   TestCaseDefinition(list(as.integer(34.7), as.integer(44.9)), 80L, 'sum 2 as.integers confused with sum of rounded value as expectation')
      # ),

      failure = list(
        TestCaseDefinition(list(34L, 44.5), 78L, 'sum 1 integer and 1 double'),
        TestCaseDefinition(list(34L, Inf), Inf, 'sum 1 integer and 1 Inf'),
        TestCaseDefinition(list(34L, NaN), NaN, 'sum 1 integer and 1 NAN'),
        TestCaseDefinition(list(34L, NA), NA, 'sum 1 integer and 1 NA'),
        TestCaseDefinition(list(c(34L, 35L), 44L), c(78L, 79L), 'sum a vector of 2 integers with 1 integer')
      ),
      tmp = list(
        TestCaseDefinition(list(3+5i, 44), , 'sum a vector of 1 complex integers with 1 integer')
      )
    )
  )

  label <- 'erroneous test case definition: wrong type, not a data.table'

  self
}
