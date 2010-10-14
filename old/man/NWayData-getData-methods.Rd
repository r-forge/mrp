\name{getData-methods}
\docType{methods}
\alias{getData-methods}
\alias{getData,NWayData-method}
\title{ ~~ Methods for Function getData  ~~}
\description{~~ Methods for function \code{getData}  ~~}
\section{Methods}{
\describe{

\item{\code{signature(object = "NWayData")}}{ Returns the flattened data as a data.frame. 
	This data.frame will have two columns for the response: "response.yes" and "response.no".
	There will be a corresponding column for each of the n ways of the data. 
	This data structure will be used for multilevel regression.
 	If group level variables or additional hierarchical variables are required, it should
 	be augmented to this data. }
}}
\keyword{methods}
