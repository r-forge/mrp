\name{mr-methods}
\docType{methods}
\alias{mr}
\alias{mr,mrp-method}
\title{Multilevel Regression Step}
\description{ This method runs the multilevel regression step of MRP. 
	The model is fit based on the \code{data} and \code{formula} slots.
	The current version uses \code{\link[stats]{glm}} and a \code{quasibinomial}
    family. The method returns a mutated \code{mrp} object.
}
\usage{
	object <- mr (object)
}
\arguments{
	\item{object}{the \code{mrp} object}
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
\value{
	A modified mrp object is returned with the multilevel regression step performed.
}
\keyword{methods}