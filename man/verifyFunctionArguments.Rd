\name{verifyFunctionArguments}
\alias{verifyFunctionArguments}
\title{Function verifyFunctionArguments}
\description{
Use this function to verify function arguments.
}
\usage{
verifyFunctionArguments(arguments_l, abort_b_1 = TRUE, verbosity_b_1 = FALSE)
}
\arguments{
\item{arguments_l}{An unconstrained list, representing the arguments. Should
always result from a call to \code{mget(ls())}.}
\item{abort_b_1}{A single boolean value stating if processing abortion should
be triggered in case of error}
\item{verbosity_b_1}{A single boolean value.}
}
\details{
This function allows to check all parameter types and values in a single
line of code.

See examples below to know how to put this function in action.
}
\value{
Value depends on parameter \code{abort_b_1} value.

When set to \code{TRUE}, any error will abort processing by issuing a call to
\code{\link{stop}} function.

When set to \code{FALSE}, returned value is a boolean. It is \code{TRUE} only
when no error have been detected. Otherwise \code{FALSE}.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\note{
 This function whenever used, should be the first statement of your function code.

 Using this function outside function code is a non-sense.
}
\examples{
fun <- function(values_i_3m) {
  verifyFunctionArguments(mget(ls()), FALSE, FALSE)
}

fun(1)
# [1] FALSE

fun(1:7)
# [1] TRUE

nonOPFun <- function(x) {
  verifyFunctionArguments(mget(ls()), FALSE, TRUE)
}

nonOPFun(1:7)
# $x
# [1] 1 2 3 4 5 6 7
#
# x FALSE unknown suffix, [NA]
#
# [1] FALSE

# real use case with abortion
myFunWithAbortion <- function(values_i_3m) {
  verifyFunctionArguments(mget(ls()))
  # ...
}

tryCatch(myFunWithAbortion(1), error = function(e) cat(e$message, '\n'))
# argument mistmatch [values_i_3m] wrong length, was expecting [3m] , got [1]

# real use case without abortion
myFunWithoutAbortion <- function(values_i_3m) {
  if (!verifyFunctionArguments(mget(ls()), FALSE)) return(FALSE)
  cat('continuing processing ...\n')
  TRUE
}

myFunWithoutAbortion(1)
# FALSE

myFunWithoutAbortion(1:3)
# continuing processing ...
# TRUE

}
\keyword{function}
\encoding{UTF-8}
