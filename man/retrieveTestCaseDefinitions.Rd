\name{retrieveTestCaseDefinitions}
\alias{retrieveTestCaseDefinitions}
\title{
Retrieve test cases definitions
}
\description{
From an instrumented class, retrieve the test case definitions.
}
\usage{
retrieveTestCaseDefinitions(object_o_1,
   functionParameterTypeFactory_o_1 = retrieveFactory())
}
\arguments{
  \item{object_o_1}{the object to consider}
  \item{functionParameterTypeFactory_o_1}{ the function parameter type factory to consider}
}
\value{
A polymorphic return that is either
\item{a list}{as returned by the \code{\link{verifyObjectNames}} function}
\item{another list}{as returned by the
\code{\link{verifyFunctionReturnTypesDefinition}} function}
\item{a data table}{the test case definitions as declared in the source class}
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
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
