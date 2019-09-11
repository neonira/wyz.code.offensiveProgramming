\name{retrieveFunctionReturnTypes}
\alias{retrieveFunctionReturnTypes}
\title{
Retrieve function return types }
\description{
From an instrumented class, retrieve the function return types definition.
}
\usage{
retrieveFunctionReturnTypes(object_o_1,
functionParameterTypeFactory_o_1 = retrieveFactory())
}
\arguments{
  \item{object_o_1}{the object to consider}
  \item{functionParameterTypeFactory_o_1}{ the function parameter type factory to consider}
}

\value{
A polymorphic return that is either
\item{a list}{as returned by the \code{\link{verifyObjectNames}} function}
\item{another list}{as returned by the \code{\link{verifyFunctionReturnTypesDefinition}} function}
\item{a data table}{the function parameter types definition as declared in the source class}
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
source(system.file('code-samples/no-defs/Addition.R',
                   package = 'wyz.code.offensiveProgramming'))
source(system.file('code-samples/frt-defs/good/full/AdditionFI.R',
                   package = 'wyz.code.offensiveProgramming'))
retrieveFunctionReturnTypes(AdditionFI()) # works, renders a data.table
retrieveFunctionReturnTypes(Addition()) # fails, renders a list
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
