\name{p-methods}
\docType{methods}
\alias{p-methods}
\alias{p,mrp-method}
\title{Poststratify step}
\description{Provides a poststratified estimate based on the multilevel model
    and the population in the object.}
\section{Arguments}{
\describe{
    \item{\code{object}}{ Class \code{\link{mrp}}. We assume the \code{\link{mr}} step
        has already been completed. We also assume the \code{population} slot is provided.
        The dimension of the \code{population} must match the dimension of the object's
        \code{theta.hat} matrix (the estimates).}
    \item{\code{poststratification.specification}}{
        This should be a vector of binary values of length equal to \code{getNumberWays(object)}.
        This method will provide estimates for the variables marked as TRUE.
        Example: (the default) all FALSE. This will return the poststratified estimate (1 number).
        Example: the first value TRUE, rest false. This will return the poststratified estimates
            for the first variable (e.g. if the first variable is state, it will provide the
            poststratified estimate for each state).}
    }
}
\keyword{methods}