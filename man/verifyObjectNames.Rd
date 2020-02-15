\name{verifyObjectNames}
\alias{verifyObjectNames}
\title{
Verify object names}
\description{
Verify object class name, object function names, and object function parameter names,
and provides a synthesis of checks.

Proceeds also to some introspection on object to identify instrumentation of function
return types and test case definitions. Provides information about completeness of instruction,
and about missing functions and test cases.
}
\usage{
verifyObjectNames(object_o_1)
}
\arguments{
  \item{object_o_1}{ the object to be checked }
}
\value{
A \code{list} with following names
\item{class_name}{the class name of the provided object}
\item{supports_strict_compliance}{a single boolean}
\item{supports_lazy_compliance}{a single boolean}
\item{class_name_compliance}{a boolean value expression class name compliance}
\item{class_name_compliance}{a vector of booleans, where names are the function names
and values express the name compliance }
\item{class_name_compliance}{a \code{data.table} exposing the name compliance and the semanting name compliance
for each paramter}
\item{owns_function_return_type_information}{a single boolean}
\item{can_be_type_checked}{a single boolean}
\item{is_function_fully_instrumented}{a single boolean}
\item{missing_function}{a vector of uninstrumented function names}
\item{owns_test_case_definitions}{a single boolean}
\item{is_test_case_fully_instrumented}{a single boolean}
\item{missing_test_cases}{a single boolean}
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
fi <- AdditionFI()
print(verifyObjectNames(fi))
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
