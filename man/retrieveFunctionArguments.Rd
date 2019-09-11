\name{retrieveFunctionArguments}
\alias{retrieveFunctionArguments}
\title{Retrieve Function Arguments.}
\description{
Use method \code{retrieveFunctionArguments} to get function or primitive arguments.
}
\usage{
retrieveFunctionArguments(fun_f_1)
}
\arguments{
\item{fun_f_1}{ a function or primitive. Not a string! }
}

\value{
A pairlist. See \code{\link{formals}}.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
# typical test on a primitive
retrieveFunctionArguments(sin)
#$x
#

# typical test on a function
retrieveFunctionArguments(ls)
#$name


#$pos
#-1L

#$envir
#as.environment(pos)

#$all.names
#[1] FALSE

#$pattern
#

#$sorted
#[1] TRUE

}
