\name{visualizeThreewayDataset}
\alias{visualizeThreewayDataset}
\alias{createLayoutMatrix}
\alias{graphsFor50States}              
\alias{visualizeThreewayDatasetWithAll}




\title{
visualizeThreewayDataset
}
\description{
visualizeThreewayDataset
}
\usage{
visualizeThreewayDataset (
        mlm, theta.hat, proportionOfVoters, 
        var1, var2, var3, 
        var2Labels=c("All", levels(var2)), var3Labels=levels(var3),
        outputFilename="maps",
        var1PlotFunction=statemaps,
        caption=paste("2000: State-level support for spending on healthcare, relative to national average of ", round (100*nationalAverage), sep=""),
        \dots)


}
\arguments{
\item{mlm}{}
\item{theta.hat}{}
\item{proportionOfVoters}{}
\item{var1}{}
\item{var2}{}
\item{var3}{}
\item{var2Labels}{}
\item{var3Labels}{}
\item{outputFilename}{}
\item{var1PlotFunction}{}
\item{caption}{}
\item{\dots}{}
}
\details{
colorblind
}
\source{

}
\references{

}
\examples{
}
\keyword{manip}
