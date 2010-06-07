\name{fitMultilevelModel-methods}
\docType{methods}
\alias{fitMultilevelModel-methods}
\alias{fitMultilevelModel,mrp-method}

\title{Fit multilevel model}

\description{Fits the multilevel model for the \code{\link{mrp}} object. The model 
    is fit based on the \code{data} and \code{formula} slots.}

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
