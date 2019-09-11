\name{verifyFunctionReturnTypesDefinition}
\alias{verifyFunctionReturnTypesDefinition}
\title{
Verify function return types definition
}
\description{
Function return types definition has to comply with many rules. This functions checks for this compliances
and helps in dectection of uncompliances.
}
\usage{
verifyFunctionReturnTypesDefinition(object_o_1,
requiresFullInstrumentation_b_1 = TRUE,
functionParameterTypeFactory_o_1 = retrieveFactory())
}
\arguments{
  \item{object_o_1}{The object to be considered}
  \item{requiresFullInstrumentation_b_1}{a boolean stating if full instrumentation is required}
  \item{functionParameterTypeFactory_o_1}{ a type factory to check parameter values and types}
}
\details{
When \code{requiresFullInstrumentation_b_1} is \code{TRUE}, each function must have an entry
in the test case parameter definition. }
\value{
A \code{list} with names
\item{validity}{a single boolean value}
\item{class}{the class name of the provided object}
\item{intent}{the stage of the failure, provides hint about the faced issue}
\item{message}{some hints to resolve the issue(s).}
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\seealso{
Refer to \code{\link{defineTestCaseDefinitionsParameterName}}.
}
\examples{
##---- typical case ----
library('data.table')
source(system.file('code-samples/frt-defs/good/full/AdditionFI.R',
                   package = 'wyz.code.offensiveProgramming'))
fi <- AdditionFI()
print(verifyFunctionReturnTypesDefinition(fi))
print(verifyFunctionReturnTypesDefinition(fi, FALSE))
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
