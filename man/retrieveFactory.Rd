\name{retrieveFactory}
\alias{retrieveFactory}
\title{
Retrieve factory
}
\description{
As factory may be modified, this function allows you to make changes and to record them in
your own specialized type factory, to match various needs and ease reuse. }
\usage{
retrieveFactory()
}
\details{
Retrieves a \code{\link{FunctionParameterTypeFactory}} from options variable named
\code{op_type_factory} or provides a default type factory.
}
\value{
An \R object that is a \code{\link{FunctionParameterTypeFactory}}.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
##---- typical case ----
ff <- retrieveFactory()
ff$addSuffix('wo', "wo class", function(o_) is(o_, "wo"))
ff$addSuffix('yo', "yo class", function(o_) is(o_, "yo"))
ff$addSuffix('zo', "zo class", function(o_) is(o_, "zo"))

options('op_type_factory' = ff)
fg <- retrieveFactory() # retrieves the factory pointed by R variable ff
fg$getRecordedTypes()[suffix \%in\% c('wo', 'yo', 'zo')] # right behavior !

# wrong behavior as retrieveFactory will provide the default factory and not yours!
options('op_type_factory' = ff)
fh <- retrieveFactory() # retrieves the default factory
fh$getRecordedTypes()[suffix \%in\% c('wo', 'yo', 'zo')]
}
\keyword{ programming }
\keyword{ utilities }
\concept{ code evaluation mode }
