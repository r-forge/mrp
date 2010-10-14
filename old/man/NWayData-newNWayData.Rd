\name{newNWayData}
\alias{newNWayData}
\alias{newNWayData,NWayData-method}
\alias{newNWayData,NWayData-constructor}
\title{
Method for constructing an NWayData object.
}
\description{
Constructs an instance of an NWayData class based on the input given.
It also flattens the data. 
}
\usage{
newNWayData(numberWays, mrp.data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{numberWays}{
	The number of ways to analyze data. This corresponds with the number of variables.
}
  \item{mrp.data}{
  	The raw data as a data.frame. The data should have numberWays+2 columns. 
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Andrew Gelman <gelman@stat.columbia.edu>,
	Daniel Lee <bearlee@alum.mit.edu>,
	Yu-Sung Su <ys463@columbia.edu>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{

%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ constructors }
