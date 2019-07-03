AdditionFI_Partial <- function() {
  self <- environment()
  class(self) <- append('AdditionFI_Partial', class(self))

  addNumeric <- function(x_n, y_n) x_n + y_n

  addDouble <- function(x_d, y_d) x_d + y_d

  addMultiDouble <- function(...) sum(..., na.rm = TRUE)

  addInteger <- function(x_i, y_i) x_i + y_i

  addMultiInteger <- function(x_i, ...) x_i + sum(..., na.rm = TRUE)

  add3length <- function(x_i_3l, y_i_3l = 1:3) x_i_3l + y_i_3l

  add3segments <- function(x_i_3l, y_i_3l = 1:3, z_i_3l) x_i_3l + y_i_3l + z_i_3l

  divideByZero <- function(x_n) x_n / 0

  generateWarning <- function(x_) 1:3 + 1:7

  generateError <- function() stop('generated error')

  function_return_types <- data.table(
    function_name = c('addNumeric', 'addDouble', 'addMultiDouble', 'addInteger', 'addMultiInteger', 'add3length',
                      'generateWarning', 'generateError', 'add3segments'),
    return_value = c('x_n', 'x_d', 'x_d', 'x_i', 'x_i', 'x_i_3l', 'x_w', 'x_e', 'x_i_3l')
  )[order(function_name)]

  self
}
