\name{findFilesInPackage}
\alias{findFilesInPackage}
\title{find files in package}
\description{
Use function \bold{\code{findFilesInPackage}} to find files in package.
}
\usage{
findFilesInPackage(filenames_s, packageName_s_1)
}
\arguments{
\item{filenames_s}{An unconstrained vector of string values.}
\item{packageName_s_1}{A length-1 vector of string values.}
}
\value{
This function is vectorized. It returns a \code{list} with one entry for each file searched for.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
# ------- example 1 -------
findFilesInPackage(c("AdditionTCFIG1.R", "Addition_TCFI_Partial_R6.R",
                     "Addition_TCFI_Partial_S3.R"),
                   "wyz.code.offensiveProgramming")
# .../wyz.code.offensiveProgramming/code-samples/both-defs/good/full/AdditionTCFIG1.R
# .../wyz.code.offensiveProgramming/code-samples/both-defs/good/partial/Addition_TCFI_Partial_R6.R
# .../wyz.code.offensiveProgramming/code-samples/both-defs/good/partial/Addition_TCFI_Partial_S3.R

# ------- example 2 -------
findFilesInPackage("datatable-intro.html", "data.table")
# .../data.table/doc/datatable-intro.html

}
\keyword{documentation}
