\name{verifyFunctionName}
\alias{verifyFunctionName}
\title{
Verify function name}
\description{
Function name must comply with a policy. This function allows to check compliance.
}
\usage{
verifyFunctionName(name_s = "aSimpleFunctionName", strictSyntax_b_1 = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name_s}{
The function name to be checked}
  \item{strictSyntax_b_1}{
A boolean value. When \code{TRUE}, allowed character set is \code{[A-Za-z0-9]+}. A
function name must start with a lowercase letter. The name is required to be camel cased,
although this cannot be checked.

When \code{FALSE}, allowed character set is \code{[A-Za-z0-9_.]+}. Classic \R function
naming applies.
}
}
\value{
A boolean value, either \code{TRUE} or \code{FALSE}
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
verifyFunctionName('matrix')
verifyFunctionName('matrix', FALSE)
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
