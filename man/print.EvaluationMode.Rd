\name{print.EvaluationMode}
\alias{print.EvaluationMode}
\title{
Print generic method for S3 class \code{EvaluationMode}
}
\description{
Prints the \code{EvaluationMode} data
}
\usage{
\method{print}{EvaluationMode}(x, ...)
}
\arguments{
  \item{x}{
    the \code{EvaluationMode} object to consider
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
b <- EvaluationMode(defineEvaluationModes()[2])
print(b)
}
\keyword{ classes }
