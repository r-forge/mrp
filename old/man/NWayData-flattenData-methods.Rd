\name{flattenData-methods}
\docType{methods}
\alias{flattenData-methods}
\alias{flattenData,NWayData-method}
\title{ ~~ Methods for Function flattenData  ~~}
\description{ ~~ Methods for function \code{flattenData}  ~~}
\section{Methods}{
\describe{

\item{\code{signature(object = "NWayData")}}{ 
    Flattens the data stored in the object so each row represents a 
	unique combination of the variables.
	This data should be augmented for other levels of 
	hierarchies and group level variables for 
	multilevel regression and poststratification.
	The data is stored as object@data and can be accessed with method getData() }
}}
\keyword{methods}
