Addition_S3_NoGeneric <- function(x_l = list()) {
  value <- x_l
  attr(value, 'class') <- 'Addition_S3_NoGeneric'
  value
}

addInteger.Addition_S3_NoGeneric <- function(x_i, y_i) x_i + y_i

addNumeric.Addition_S3_NoGeneric <- function(x_n, y_n) x_n + y_n

addDouble.Addition_S3_NoGeneric <- function(x_d, y_d) x_d + y_d

divideByZero.Addition_S3_NoGeneric <- function(x_n) x_n / 0

generateWarning.Addition_S3_NoGeneric <- function(x_ = 0L) 1:3 + 1:7 + x_

generateError.Addition_S3_NoGeneric <- function() stop('generated error')



