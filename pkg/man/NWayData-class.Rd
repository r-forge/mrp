\name{NWayData}
\Rdversion{1.1}
\docType{class}
\alias{NWayData}
\alias{NWayData-class}


\title{Represents an n-way data set}
\description{
	The \code{NWayData} class is a representation of a data set with a
	binary response variable and n variables of interest
}
\section{Object Creation}{
  Objects can be created by calls of the form \code{\link{newNWayData}} function.  
  The resulting \code{NWayData} object will be a representation of the data set that
  can be used for generalized linear regression using \link{glmer}} 
\section{Slots}{
	\describe{
		\item{\code{data}:}{On creation, this will be a \code{data.frame} with n+2 columns.
			  The number of rows will be the combinatorial expansion of the covariates}
    	\item{\code{numberWays}:}{The number of ways for this analysis}
    	\item{\code{ybarWeighted}:}{The percent of yes in the data, weighted}
    	\item{\code{n}:}{number of observations}
    	\item{\code{designEffectByCell}:}{the design effect by cell}
    	\item{\code{dataLength}:}{The length of the original data (only complete cases are counted).}
  }
}
\section{Methods}{
	No methods defined with class "NWayData" in the signature.
}
\references{ ~put references to the literature/web site here ~ }

\author{
  Andrew Gelman <gelman@stat.columbia.edu>,
  Daniel Lee <bearlee@alum.mit.edu>,
  Yu-Sung Su <ys463@columbia.edu>
}

\examples{
showClass("NWayData")
}

\keyword{classes}
