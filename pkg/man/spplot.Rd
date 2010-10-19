\name{plotmrp}
\docType{methods}
\alias{spplot,mrp-method}
\alias{panel.polygonsplot}
\alias{plotmrp}

\title{Plot poststratified estimates onto sp map objects}
\description{
Poststratify and plot a fitted \code{\link{mrp-class}} object onto a
\link[sp]{SpatialPolygonsDataFrame}. Note that package \emph{mrp} masks
the \emph{sp} function \code{\link[sp]{panel.polygonsplot}} for a single
change: to plot \code{NA} values in a color other than \sQuote{bg}.}

\usage{
\S4method{spplot}{mrp}( obj, formula, spmap=NULL, FID="state",
exclude=NULL, stroke=list(), at, cuts=15, pretty=FALSE, add.settings, \dots)
}
\arguments{
  \item{obj}{A \code{\link{mrp-class}} object.}
  \item{formula}{The desired poststratification, where the left-hand
    side indicates the name of the variable in the \code{mrp} data that
    identifies the polygons to plot. In the special case where the
    desired poststratification is \emph{only} by
    geographic unit, place it on both sides of the equation (e.g.,
    \code{state ~ state}.}
  \item{spmap}{A map of class 
    \code{\link[sp]{SpatialPolygonsDataFrame}}, such as is produced by
    \code{\link[maptools]{readShapeSpatial}} for reading ESRI \dQuote{shapefiles.}
  This package includes a US map in \link{spmap.states}.}
  \item{FID}{The name of the column in the \code{spmap} to use as unique
    \dQuote{feature IDs}, whose values to the left-hand-side of
    \code{formula}. In the included \link{spmap.states} map, the
    column with two-letter state abbreviations is called
    \sQuote{STATE}, which is the default here.}
  \item{exclude}{A vector of feature IDs to exclude from the
    plot. \emph{Note:} any rows not present in the \code{mrp} data
    itself will automatically be excluded.}
  \item{stroke}{A subscript (character, logical, or integer) vector or a
    list of such vectors, of feature IDs (logical or integer with respect to
    the order of \code{spmap}) to plot with a distinct
    stroke. The type, width, and color of the line are taken from the
    \emph{superpose.line} list, which can be customized as described
    below. In the event that a feature ID appears multiple times in the
    list, the last value is used; e.g., if it is in both the first and second
    vectors of a list, its stroke will be \code{superpose.line$lty[2]}.}
  \item{add.settings}{A list of lattice graphical parameters, suitable
    for \code{trellis.par.set()}. These will override the MRP defaults,
    which bear mention for two reasons: 1) top and left strip titles are
    drawn only in panels on the top and left of the full lattice. 2) any
    \code{NA}-valued parts of any panel will be filled in the color given
    by \sQuote{reference.line$col}, which in our theme defaults to a 68\%
    opaque black, or \code{"#00000044"}.}
  \item{subset}{Expression that evalues to a logical or integer indexing
    vector, evaluated in the \code{\link[reshape2]{melt}}ed
    poststratified results.}
  \item{at}{See \link[lattice]{levelplot}. If a vector of values is
    provided to \code{at}, it overrides \code{cuts}, \code{pretty}, and
    \code{center}.}
  \item{cuts}{See \link[lattice]{levelplot}. If using a
    \code{\link[RColorBrewer]{RColorBrewer}} palette, should be one less
  than the number of colors in the call to \code{\link[RColorBrewer]{brewer.pal}}.}
  \item{pretty}{See \link[lattice]{levelplot}}
  \item{center}{Value at which to center the calculation of
    limits. Default=0.5}
  \item{between}{Documented for \code{\link{xyplot}}. A named list
    giving space between panels (in character heights). Default is 0.25
    for both \eqn{x} and \eqn{y}.}

  \item{\dots}{further arguments to be passed to
    \code{\link[sp]{spplot}} which in turn passes them to
    \code{\link[lattice]{levelplot}}.}
}
\value{
An object of class \code{"trellis"}. See \code{\link[lattice]{xyplot}}
for details. }
  \seealso{
  }
  \examples{
## plot the example from mrp()
#library(MRP);example(mrp)
library(MRP);load("~/Desktop/fitted.Rdata")
    ## Dimension of plot are by order of terms -- y,x so
## this will plot age along x and poll along y.
spplot(mrp.simple, state~ age+poll,
subset=age=="18-29",
spmap.states, "STATE", exclude=c("AK","DC","HI"),
cuts=9, 
add.settings=list(
  regions=list(col=RColorBrewer:::brewer.pal(10,"BrBG")),
  superpose.line=list(col="#00000044",lwd=c(.8,.5)),
  add.text=list(cex=.7,fontface="italic")),
stroke=list(c("IA","NH","VT","CT","MA","ME")),
colorkey=list(
  space="bottom",height=.5,width=.5)
)

\dontrun{


}
}
  \keyword{methods}
