\name{matchFunctionSignature}
\alias{matchFunctionSignature}
\title{Retrieve Function Arguments.}
\description{
Use method \code{matchFunctionSignature} to match a function signature.
}
\usage{
matchFunctionSignature(aFunction_f_1, aFunctionTemplate_f_1 = function(){})
}
\arguments{
\item{aFunction_f_1}{ a function or primitive. Not a string! }
\item{aFunctionTemplate_f_1}{ a function or primitive to be used as model. Not a string! }
}
\value{
A \code{boolean} value.
}
\details{
To get \code{TRUE} as result, function and function model must share exactly the
sames attributes names and values, including default values if any used.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
matchFunctionSignature(sum, function(..., na.rm = FALSE) { NULL })
# [1] TRUE

matchFunctionSignature(sum, function(..., na.rm) { NULL })
#[1] FALSE
}
