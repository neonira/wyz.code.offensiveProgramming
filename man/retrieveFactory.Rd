\name{retrieveFactory}
\alias{retrieveFactory}
\title{
Retrieve factory
}
\description{
As factory may be modified, this function allows you to make changes and to record them in
a global environment variable, to ease reuse. }
\usage{
retrieveFactory(functionParameterTypeFactory_o_1 = NULL)
}
\arguments{
  \item{functionParameterTypeFactory_o_1}{ The factory you want to use. Will provide the
  standard factory by default. }
}
\details{
Sets typeFactory in \code{.GlobalEnvironment} in order to reuse it transparently wherever required}
\value{
An object that is an \R \code{FunctionParameterTypeFactory}.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\seealso{
Refer to \code{\link{FunctionParameterTypeFactory}}
}
\examples{
##---- typical case ----
ff <- FunctionParameterTypeFactory()
ff$addSuffix('wo', "wo class", function(o_) is(o_, "wo"))
ff$addSuffix('yo', "yo class", function(o_) is(o_, "yo"))
ff$addSuffix('zo', "zo class", function(o_) is(o_, "zo"))
retrieveFactory(ff) # set and retrieves the factory
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
