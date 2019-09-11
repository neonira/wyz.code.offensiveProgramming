\name{getObjectClassKind}
\alias{getObjectClassKind}
\title{
Get \R object class kind
}
\description{
Get the class kind of an \R object as a string.
}
\usage{
getObjectClassKind(object_o_1)
}
\arguments{
  \item{object_o_1}{ the object to analyze. See \code{\link{is.object}}.}
}
\value{
A single \code{character} value, taken in set \code{"S3"}, \code{"S4"},
\code{"RC"}, \code{"R6"}, \code{"environment"}, \code{"unknown"}.

When provided \code{object_} is not an \R object, then value \code{NA_character_}
is returned.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
##---- typical case ----
getObjectClassKind(new.env())
# [1] NA

myrc <- setRefClass("RC",
  fields = list(x = "numeric"),
  methods = list(
    initialize = function(x = 1) .self$x <- x,
    getx = function() x,
    inc = function(n = 1) x <<- x + n
  )
)

getObjectClassKind(myrc$new())
# [1] RC

myr6 <- R6::R6Class("R6",
  public = list(
    x = NULL,
    initialize = function(x = 1) self$x <- x,
    getx = function() self$x,
    inc = function(n = 1) self$x <- x + n
  )
)

getObjectClassKind(myr6$new())
# [1] R6

}
\keyword{ programming }
\keyword{ utilities }
\concept{ class management }
