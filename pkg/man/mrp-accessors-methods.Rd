\name{mrp-accessors}
\docType{methods}
\alias{mrp-accessors-methods}

\alias{getData,mrp-method}
\alias{getNumberWays,mrp-method}
\alias{getPopulation,mrp-method}
\alias{getFormula,mrp-method}
\alias{setPopulation,mrp,array-method}
\alias{setFormula,mrp,character-method}

\title{Accessors for the MRP object}
\description{These are the accessors for the MRP object.}
\usage{
	\code{
		getData(object)
		getNumberWays(object)
		getPopulation(object)
		getFormula(object)
		
		setPopulation(object, population)
		setFormula(object, formula)}
}
\arguments{
	\item{object}{the \code{mrp} object.}
	\item{population}{the population distribution of the data (class \code{array}). 
		This is intended to be a probability mass table, but will also work with unnormalized population distributions.}
	\item{formula}{the formula for the multilevel regression (class \code{character}). The left side of the formula
		should be 	}
	
}
\value{
	\item{}{}
}
\keyword{methods}
