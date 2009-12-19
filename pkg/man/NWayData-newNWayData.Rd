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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Andrew Gelman <gelman@stat.columbia.edu>,
	Daniel Lee <bearlee@alum.mit.edu>,
	Yu-Sung Su <ys463@columbia.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (numberWays, mrp.data) {
    if ("data.frame" != class (mrp.data)) {
        stop ("Correct usage (data.frame): <response> <var 1> <var 2> ... <var n> <weights>\n")
    } 
    if (numberWays + 2 != ncol(mrp.data)) {
        stop (cat ("mrp.data must have", numberWays+2, "columns. Found: ", length(mrp.data), "\n",
                                           "Correct usage (data.frame): <response> <var 1> <var 2> ... <var n> <weights>\n"))
    }
    dims <- lapply (mrp.data[,2:(1+numberWays)], nlevels)
    dimLevels <- lapply (mrp.data[,2:(1+numberWays)], levels)
    varNames <- names (mrp.data)
    
    ## reduce data set to remove NAs
    completeCases <- complete.cases (mrp.data)
    mrp.data <- mrp.data[completeCases,]
    
    matchingSubsets <- list()
    for (i in 1:numberWays) {
        matchingSubsets[[varNames[i+1]]] <- lapply (dimLevels[[i]], "==", mrp.data[ , i+1])
    }
    
    ## change the response variable to a binary variable with 1 for Yes and 0 for No: 
    ## as.numeric() should make the positive response 1 and the negative response 2.
    y <- 2-as.numeric(mrp.data[ , varNames[1]]) 
    
    ybarWeighted       <- array (NA, dim=dims, dimnames=dimLevels) 
    n                  <- array (0, dim=dims, dimnames=dimLevels)
    designEffectByCell <- array (0, dim=dims, dimnames=dimLevels)
    
    subset <- rep (TRUE, nrow(mrp.data))
    for (ii in 1:length(ybarWeighted)) {
        subset <- rep (TRUE, nrow(mrp.data))
        carry <- 1
        for (jj in 1:numberWays) {
            ind <- ((ii-1) \%/\% carry) \%\% dims[[jj]] + 1
            carry <- carry * dims[[jj]]
            subset <- subset & matchingSubsets[[jj]][[ind]] 
        }
        ybarWeighted[ii] <- weighted.mean (y[subset], mrp.data[subset , numberWays + 2])
        n[ii] <- sum (subset)
        designEffectByCell[ii] <- 
                ifelse (n[ii]==1, 1,
                        1 + var (mrp.data[subset, numberWays+2] / mean(mrp.data[subset, numberWays+2])))
    }
    ybarWeighted[is.nan(ybarWeighted)] <- NA
    
    nWayData <- new ("NWayData", 
            numberWays=numberWays,
            ybarWeighted = ybarWeighted, 
            n=n,
            designEffectByCell=designEffectByCell, 
            dataLength=sum(completeCases))
    nWayData <- flattenData (nWayData) 
    return (nWayData)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ constructors }
