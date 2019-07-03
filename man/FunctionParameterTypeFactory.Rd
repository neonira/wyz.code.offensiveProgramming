\name{FunctionParameterTypeFactory}
\alias{FunctionParameterTypeFactory}
\title{
Function parameter type factory
}
\description{
This factory is a parameter type check factory. It provides type checking for each allowed type.
}
\usage{
FunctionParameterTypeFactory()
}
\details{
Many common types are already recorded and available through the factory. Use the function
\code{getRecordedTypes}  to get more insight.

If you desire to verify a type instrumentation, just use \code{checkSuffix} function.
If you want to add an instrumentation for a new type, use \code{addSuffix} function.

See examples below for more hands-on approach.
}
\value{
An object that is an \R \code{environment}.
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
ff <- FunctionParameterTypeFactory()
ff$checkSuffix('b') # TRUE

# see verify_function recorded for 'boolean' entries
ff$getRecordedTypes()[suffix == 'b']$verify_function[[1]]

# record a new entry for suffix 'wo'
ff$addSuffix('wo', "wo class", function(o_) is(o, "wo")) # TRUE
ff$getRecordedTypes()[suffix == 'wo']
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
