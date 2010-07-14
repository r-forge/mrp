\name{mr-methods}
\docType{methods}
\alias{mr}
\alias{mr,mrp-method}
\title{Multilevel regression step}
\description{ This method fits the multilevel model if one does not exist. In order
    to force a multilevel model to be fit (and for more details), see 
    \code{\link{fitMultilevelModel}}.}
\section{Requirements}{
\describe{
    The object is assumed to have been created using the \code{\link{newMrp}} constructor.
    A valid formula is required (the names of the formula matches that in the data slot).
    }
}
\keyword{methods}