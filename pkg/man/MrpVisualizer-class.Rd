\name{MrpVisualizer-class}
\Rdversion{1.1}
\docType{class}
\alias{MrpVisualizer-class}

\title{MrpVisualizer, the default visualizer for MRP}
\description{
  This class provides basic visualization for an mrp object.
  The MrpVisualizer class is also intended to be extended for
  custom visualization of the mrp objects. 
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("MrpVisualizer", ...)}.
  This will build a visualizer with default settings, which will output to screen.
}
\section{Slots}{
   \describe{
    \item{\code{filename}:}{The output file name of the visualization, if the output is to a file (class \code{"character"}).}
    \item{\code{height}:}{The height of the visualization (class \code{"numeric"}).}
    \item{\code{width}:}{The width of the visualization (class \code{"numeric"})}
    \item{\code{shrinkVertical}:}{The proportion to shrink vertical space by relative to horizontal space (class \code{"numeric"}).}
    \item{\code{parameters}:}{Parameters given to the function \code{par} (class \code{"list"}).}
    \item{\code{plot.var1}:}{This function should be able to plot a vector of the length of var1 levels (class \code{"function"}). 
      This function takes 3 parameters: \code{data}, \code{average}, \code{proportions}. The \code{data} is a vector, \code{average} is 
      a numeric value, and \code{proportions} is a vector of the same length as the data. The default implementation just plots the data
      and ignores the other two parameters.}
    \item{\code{graphics.open}:}{A function to open the graphics device (class \code{"function"}). This can be replaced with functions such
      as \code{pdf} and \code{png}.}
    \item{\code{graphics.close}:}{A function to close the graphics device (class \code{"function"}). In most cases, this should be \code{dev.off}.}
  }
}
\section{Methods}{
  No methods defined with class \code{MrpVisualizer} in the signature.
}
\references{}

\author{
  Andrew Gelman <gelman@stat.columbia.edu>,
  Daniel Lee <bearlee@alum.mit.edu>,
  Yu-Sung Su <ys463@columbia.edu>
}


\examples{
  showClass("MrpVisualizer")
}

\keyword{classes}
