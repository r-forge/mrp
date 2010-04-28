\name{setFormula-methods}
\docType{methods}
\alias{setFormula-methods}
\alias{setFormula,mrp-method}
\title{Sets the formula of an \code{\link{mrp}} object}
\description{This method sets the formula of the \code{mrp} object.}
\section{Fit Multilevel Model}{
\describe{
    The multilevel model is fit using the object's \code{formula} and \code{data}.
    The current version uses \code{\link[lme4]{glm}} and a \code{quasibinomial}
    family. The method returns a mutated \code{mrp} object.}
}
\section{Mutations to the object}{
\describe{
    These are the changes to the object:
    \item{\code{multilevelModel}:}{ Class \code{mer}. This is the representation
        of the multilevel model as returned by \code{glmer}}.
    \item{\code{theta.hat}:}{ These are the estimated values. The dimension 
        will match the dimension of the data (see \code{\link{getYbarWeighted}}).}
    }
}
\keyword{methods}