\name{retrieveFunctionArgumentNames}
\alias{retrieveFunctionArgumentNames}
\title{Retrieve Function Argument Names.}
\description{
Use method \code{retrieveFunctionArgumentNames} to get names of function or
primitive arguments.
}
\usage{
retrieveFunctionArgumentNames(fun_f_1)
}
\arguments{
\item{fun_f_1}{ a function or primitive. Not a string! }
}

\value{
A vector. See \code{\link{formalArgs}}.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
# typical test on a primitive
retrieveFunctionArgumentNames(sin)
#[1] "x"

# typical test on a function
retrieveFunctionArguments(ls)
#[1] "name"      "pos"       "envir"     "all.names" "pattern"   "sorted"
}
