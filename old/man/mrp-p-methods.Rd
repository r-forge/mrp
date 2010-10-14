\name{p-methods}
\docType{methods}
\alias{p}
\alias{p-methods}
\alias{p,ANY,ANY-method}
\alias{p,mrp-method}
\title{Poststratify step}
\description{Provides a poststratified estimate based on the multilevel model
    and the population in the object.}
\usage{
	p (object, poststratification.specification)
}
\arguments{
    \item{object}{ Class \code{\link{mrp}}. We assume the \code{\link{mr}} step
        has already been completed. We also assume the \code{population} slot is provided.
        The dimension of the \code{population} must match the dimension of the object's
        \code{theta.hat} matrix (the estimates).}
    \item{poststratification.specification}{
        This should be a vector of binary values of length equal to \code{getNumberWays(object)}.
        This method will provide estimates for the variables marked as TRUE.
        Example: (the default) all FALSE. This will return the poststratified estimate (1 number).
        Example: the first value TRUE, rest false. This will return the poststratified estimates
            for the first variable (e.g. if the first variable is state, it will provide the
            poststratified estimate for each state).}
}
\value{
	Values representing the poststratified estimate for the dimension(s) indicated.
}
\keyword{methods}