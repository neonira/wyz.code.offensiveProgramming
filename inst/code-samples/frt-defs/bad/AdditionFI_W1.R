AdditionFI_W1 <- function() {
  self <- environment()
  class(self) <- append('AdditionFI_W1', class(self))

  addNumeric <- function(x_n, y_n) x_n + y_n

  addDouble <- function(x_d, y_d) x_d + y_d

  addInteger <- function(x_i, y_i) x_i + y_i

  divideByZero <- function(x_n) x_n / 0

  generateWarning <- function(x_) 1:3 + 1:7

  generateError <- function() stop('generated error')

  function_return_types <- data.table(
    function_name = c('addNumeric', 'addDouble', 'addInteger', 'addInteger', 'addMultiInteger', 'generateWarning', 'generateError'),
    return_value = c('x_n', 'x_d', 'x_i', 'x_i_1', 'x_i', 'x_i', 'x_')
  )

  label <- 'erroneous function return type definition - provided function names are not unique'

  self
}
