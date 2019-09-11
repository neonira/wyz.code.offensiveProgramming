\name{getObjectFunctionNames}
\alias{getObjectFunctionNames}
\alias{getClassTypicalFunctionNames}
\title{Retrieve Function Names From Object}
\description{
Use method \code{getObjectFunctionNames} to get the function names of an object (
see \code{is.object}).
)
}
\usage{
getObjectFunctionNames(object_o_1, allNames_b_1 = FALSE)

getClassTypicalFunctionNames(object_o_1)
}
\arguments{
\item{object_o_1}{ the object to analyze. }
\item{allNames_b_1}{ A boolean value. When \code{TRUE}, uses \code{getClassTypicalFunctionNames}
to restrict the set of function names returned.}
}
\details{
Function \code{getClassTypicalFunctionNames} gives back function names that are related to
\R class style, and automatically added by \R to your class object.
}
\value{
A \code{vector} of function names (\code{character}).
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

getObjectFunctionNames(MyEnv())
# [1] "f"

# another test
getObjectFunctionNames(new.env())
#[1] NA

}
