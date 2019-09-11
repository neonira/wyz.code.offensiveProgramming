\name{runTestCase}
\alias{runTestCase}
\title{
Run test cases
}
\description{
Run specified test cases under the given evaluation mode
}
\usage{
runTestCase(object_o_1, testCaseIndexes_i, evaluationMode_o_1 = EvaluationMode())
}
\arguments{
  \item{object_o_1}{The \R object to consider}
  \item{testCaseIndexes_i}{a vector of numbers identifying the test cases to run}
  \item{evaluationMode_o_1}{the evaluation mode to use. see \code{\link{EvaluationMode}}}
}
\value{
A \code{list} with two names
\item{raw}{ a \code{list} with one entry for each test ran, holding all data and metadata
related to the test}
\item{synthesis}{a summary \code{data.table} that allows to see at a glance all the tests
results. Also eases comparisons of results between various evaluation modes.}
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
source(system.file('code-samples/both-defs/good/full/AdditionTCFI_G1.R',
                   package = 'wyz.code.offensiveProgramming'))
em <- EvaluationMode('type_checking_enforcement')
runTestCase(AdditionTCFI_G1(), c(3, 5, 7), em)
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
