#' a sample class
#' @return environment
#' @examples
#' a <- Addition()
Addition <- function() {
  self <- environment()
  class(self) <- append('Addition', class(self))

  addNumeric <- function(x, y_n) x + y_n

  addDouble <- function(x_d, y_d) x_d + y_d

  addInteger <- function(x_i, y_i) x_i + y_i

  divideByZero <- function(x_n) x_n / 0

  generateWarning <- function(x_ = 0L) 1:3 + 1:7 + x_

  generateError <- function() stop('generated error')

  self
}
