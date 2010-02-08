\name{getFormula-methods}
\docType{methods}
\alias{getFormula-methods}
\alias{getFormula,mrp-method}
\title{Get formula for the multilevel model}
\description{Returns the current formula for the \code{\link{mrp}} object.}
\section{Formula}{
\describe{
    The formula is the string representation of the model as specified by
    \code{\link[lme4]{glmer}}.
    
    The multilevel model is fit with the formula by the 
    \code{\link{fitMultilevelModel}} method. This formula should have 
    \code{"cbind (response.yes, response.no)"} as the left side of the formula,
    unless the data in the \code{data} slot is modified by hand. The names of
    the formula for the right hand side should be the names of the variables
    in the \code{data} slot.}
}
\keyword{methods}