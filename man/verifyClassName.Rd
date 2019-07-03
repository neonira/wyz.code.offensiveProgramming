\name{verifyClassName}
\alias{verifyClassName}
\title{
Verify class name}
\description{
Class name must comply with a policy. This function allows to check compliance.
}
\usage{
verifyClassName(name_s = "MyClassName", strictSyntax_b_1 = TRUE)
}
\arguments{
  \item{name_s}{
The class name to be checked}
  \item{strictSyntax_b_1}{
A boolean value. When \code{TRUE}, allowed character set is \code{[A-Za-z0-9]+}. A
class name must start with an uppercase letter. The name is required to be camel cased,
although this cannot be checked.

When \code{FALSE}, allowed character set is \code{[A-Za-z0-9_.]+}. Classic \R class
naming applies.}
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
verifyClassName('matrix')
verifyClassName('matrix', FALSE)
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
