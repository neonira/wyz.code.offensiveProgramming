\name{EvaluationMode}
\alias{EvaluationMode}
\title{
Evaluation mode definition
}
\description{
Class to define your evaluation mode}
\usage{
EvaluationMode(value_s_1 = defineEvaluationModes()[2])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value_s_1}{ one string that must come from \code{\link{defineEvaluationModes}}
}
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
EvaluationMode(defineEvaluationModes()[3])
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
