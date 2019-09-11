\name{print.TestCaseDefinition}
\alias{print.TestCaseDefinition}
\title{
Print generic method for S3 class \code{TestCaseDefinition}
}
\description{
Prints the \code{TestCaseDefinition} data
}
\usage{
\method{print}{TestCaseDefinition}(x, ...)
}
\arguments{
  \item{x}{
    the \code{TestCaseDefinition} object to consider
  }
  \item{\ldots}{
    any other argument, passed to print.
  }
}
\author{
  \packageAuthor{wyz.code.offensiveProgramming}

  Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
b <- TestCaseDefinition(list(1L, 2L), 3L, 'sum of 2 integers')
print(b)
}
\keyword{ classes }
