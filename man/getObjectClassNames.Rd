\name{getObjectClassNames}
\alias{getObjectClassNames}
\alias{hasMainClass}
\title{Retrieve Function Arguments.}
\description{
Use method \code{getObjectClassNames} to get the class names of an object (
see \code{is.object}).
)
}
\usage{
getObjectClassNames(object_o_1)
hasMainClass(object_o_1, classname_s_1)
}
\arguments{
\item{object_o_1}{ the object to analyze. }
\item{classname_s_1}{ the class to match the \code{classname} entry of the
returned value of \code{getObjectClassNames). } }
}

\value{
A \code{list} with two character entries. First one is named \code{classname},
provides the main classname (the one found in first position). Second one is named
\code{classnames}, provides all the class names born by the \code{object_}.
}
\author{
\packageAuthor{wyz.code.offensiveProgramming}

Maintainer: \packageMaintainer{wyz.code.offensiveProgramming}
}
\examples{
# typical test
getObjectClassNames(getObjectClassNames(factor(letters[1:3])))
#$classname
#[1] "factor"

#$classnames
#[1] "factor"

# another test
getObjectClassNames(new.env())
#$classname
#[1] NA

#$classnames
#[1] "environment"

}
