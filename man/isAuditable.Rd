\name{isAuditable}
\alias{isAuditable}
\title{Is auditable.}
\description{
Use method \code{isAuditable} to know if code is auditable
}
\usage{
isAuditable()
}
\value{
A boolean value. To turn value to \code{TRUE}, environment variable
\code{OP_AUDITABLE} must be set to a value that does not match an empty string.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
# typical case
isAuditable()
# FALSE
}
