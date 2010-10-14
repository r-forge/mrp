\name{newMrp}
\alias{newMrp}
\title{Create a new MRP object}
\description{
	Creates a new MRP object from data.
}
\usage{
	newMrp (response, vars, population, weight=rep(1, length(response)))
}
\arguments{
	\item{response}{a vector where values are of two levels indicating the response (the first level is considered the positive response)}
	\item{vars}{the variates for the data. each of the variates should be factors}
	\item{population}{the population to be used in poststratification}
	\item{weight}{the weight of each response}
}
\value{
	Returns a valid MRP object.
}
