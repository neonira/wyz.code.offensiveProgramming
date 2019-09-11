\name{getObjectFunctionArgumentNames}
\alias{getObjectFunctionArgumentNames}
\title{Retrieve Function Arguments.}
\description{
Use method \code{getObjectFunctionArgumentNames} to get the function argument names
of an object (see \code{is.object}).
}
\usage{
getObjectFunctionArgumentNames(object_o_1, allNames_b_1 = TRUE)
}
\arguments{
\item{object_o_1}{ the object to analyze. }
\item{allNames_b_1}{ A boolean value. Passed to function \code{getObjectFunctionNames} to
restrict output if needed.}
}

\value{
A \code{list}. Entries are named with function names. Each entry is of type character,
and holds function argument names. Could be empty if function takes no argument.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
# typical test
MyEnv <- function() {
     self <- environment()
     class(self) <- append('MyEnv', class(self))
     f <- function(x_3, y_3n) x_3 + y_3n
     self
}

getObjectFunctionArgumentNames(MyEnv())
#$f
#[1] "x_3" "y_3n"

}
