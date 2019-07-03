\name{TestCaseDefinition}
\alias{TestCaseDefinition}
\title{
Test case definition
}
\description{
Defines a test case
}
\usage{
TestCaseDefinition(params_l, expectedResult_, description_s_1)
}
\arguments{
  \item{params_l}{ a list that holds the test case input values }
  \item{expectedResult_}{ test case expected result. This will be used to compare with
  function execution results}
  \item{description_s_1}{ a single string, test case description
}
}
\details{
Test case definition takes sense only when intimately correlated with a function
}
\value{
An object that is an \R \code{environment}.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\seealso{
Refer to \code{\link{FunctionParameterTypeFactory}}
}
\examples{
##---- typical case ----
tcd <- TestCaseDefinition(list(1:5), 15, 'sum of 5 first non nul integers')
tcd <- TestCaseDefinition(list(1:7, 3:5, sample(1:100, 19, FALSE)),
                          list(3:5), 'extract smallest length from input')
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
