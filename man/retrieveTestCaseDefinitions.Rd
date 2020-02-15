\name{retrieveTestCaseDefinitions}
\alias{retrieveTestCaseDefinitions}
\alias{retrieveTestCaseDescriptions}
\title{
Retrieve test case definitions or test case descriptions.
}
\description{
From an instrumented class, retrieve the test case definitions or descriptions.
}
\usage{
retrieveTestCaseDefinitions(object_o_1)
retrieveTestCaseDescriptions(object_o_1)
}
\arguments{
  \item{object_o_1}{the object to consider}
}
\value{
For \code{function}, \emph{retrieveTestCaseDefinitions}, a polymorphic return that is either
\item{a list}{as returned by the \code{\link{verifyObjectNames}} function}
\item{another list}{as returned by the
\code{\link{verifyFunctionReturnTypesDefinition}} function}
\item{a data table}{the test case definitions as declared in the source class}

For \code{function}, \emph{retrieveTestCaseDescriptions}, either a string when
no test case definitions exists or a data.table of the test case descriptions.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\seealso{
Refer to \code{\link{defineEvaluationModes}}
}
\examples{
##---- typical case ----
library('data.table')
source(system.file('code-samples/tcd-defs/good/partial/AdditionTCPartial.R',
                   package = 'wyz.code.offensiveProgramming'))
source(system.file('code-samples/no-defs/Addition.R',
                   package = 'wyz.code.offensiveProgramming'))
retrieveTestCaseDefinitions(AdditionTCPartial()) # works, renders a data.table
retrieveTestCaseDefinitions(Addition()) # fails, renders a list

retrieveTestCaseDescriptions(Addition())
retrieveTestCaseDescriptions(AdditionTCPartial())
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
