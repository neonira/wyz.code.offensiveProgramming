\name{retrievePackageFunctionNames}
\alias{retrievePackageFunctionNames}
\title{
Retrieve Package Function Names
}
\description{
Get the function names from a package name
}
\usage{
retrievePackageFunctionNames(packageName_s_1, libraryPath_s_1 = .libPaths()[1])
}
\arguments{
  \item{packageName_s_1}{ the package name to seek for }
  \item{libraryPath_s_1}{ the folder to look for }
}
\value{
A \code{vector} of function names
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
retrievePackageFunctionNames('wyz.code.offensiveProgramming')
}

