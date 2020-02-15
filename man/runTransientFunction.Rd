\name{runTransientFunction}
\alias{runTransientFunction}
\title{
Run transient function
}
\description{
Run a function in a transient (non persistent) context.
}
\usage{
runTransientFunction(function_f_1,
                     arguments_l,
                     evaluationMode_o_1,
                     function_return_type_s_1)
}
\arguments{
  \item{function_f_1}{ a single \R function}
  \item{arguments_l}{ a list of arguments to pass to the function}
  \item{evaluationMode_o_1}{ an evaluation mode object. See \code{\link{EvaluationMode}}}
  \item{function_return_type_s_1}{ a semantic parameter name, given as a string, to express expected function return type}
}
\value{
A \code{list} with names
\item{status}{ a single boolean. Always \code{TRUE} when evaluation mode is \code{"standard_R_evaluation"}.
Otherwise, will reflect result validity in the chose evaluation mode. }
\item{value}{the result of the computation, might be a scalar or not, a warning, an error, ... }
\item{mode}{the evaluation mode used to check the results}
\item{function_return_type_check}{available if mode is different of \code{"standard_R_evaluation"} }
\item{parameter_type_checks}{available if mode is \code{"type_checking_inforcement"} }
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\seealso{
Refer to \code{\link{runFunction}}
}
\examples{
##---- typical case ----
em <- EvaluationMode(defineEvaluationModes()[3])
h <- function(x_s) x_s
runTransientFunction(h, list('neonira'), em, 'x_s')
runTransientFunction(h, list(pi), em, 'x_s')
runTransientFunction(h, list(pi), em, 'x_d')
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }