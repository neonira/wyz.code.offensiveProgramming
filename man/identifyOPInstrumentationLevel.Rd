\name{identifyOPInstrumentationLevel}
\alias{identifyOPInstrumentationLevel}
\title{Identify Offensive Programming Instrumentation Level }
\description{
Provide short information about offensive programming instrumentation level
}
\usage{
identifyOPInstrumentationLevel(object_o_1 = NULL,
                               methodName_s_1 = NA_character_,
                               typeFactory_o_1 = retrieveFactory())
}
\arguments{
  \item{object_o_1}{ the object to be checked }
  \item{methodName_s_1}{ the method to consider, if any. }
  \item{typeFactory_o_1}{ the type factory to check parameter types }
}
\value{
A \code{list} with following names
\item{offensive_programming}{a single boolean}
\item{full_instrumentation}{a single boolean}
\item{semantic_naming}{a single boolean}
\item{function_return_type}{a single boolean}
\item{test_case_definition}{a single boolean}
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\seealso{
Refer to \code{\link{verifyClassName}}and \code{\link{verifyFunctionName}}.
}
\examples{
##---- typical case ----
library('data.table')
source(system.file('code-samples/frt-defs/good/full/AdditionFI.R',
                   package = 'wyz.code.offensiveProgramming'))
identifyOPInstrumentationLevel(AdditionFI())

#$offensive_programming
#[1] TRUE

#$full_instrumentation
#[1] FALSE

#$semantic_naming
#[1] TRUE

#$function_return_type
#[1] TRUE

#$test_case_definition
#[1] FALSE
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
