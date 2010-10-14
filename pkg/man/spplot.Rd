\name{plotmrp}
\docType{methods}
\alias{spplot,mrp-method}
\alias{plotmrp}

\title{Plot poststratified estimates onto sp map objects}
\description{
Poststratify and plot a fitted \code{\link{mrp-class}} object onto a \link[sp]{SpatialPolygonsDataFrame}.
}
\usage{
\S4method{spplot}{mrp}( obj, formula, spmap=NULL, FID="state",
exclude=NULL, add.settings,\dots)
}
\arguments{
  \item{obj}{A \code{\link{mrp-class}} object.}
  \item{formula}{The desired poststratification, where the left-hand
    side indicates the name of the variable in the \code{mrp} data that
    identifies the polygons to plot (e.g., \sQuote{state}).}
  \item{spmap}{A map of class 
    \code{\link[sp]{SpatialPolygonsDataFrame}}, such as is produced by
    \code{\link[maptools]{readShapeSpatial}} for reading ESRI \dQuote{shapefiles.}
  This package includes a US map in \link{spmap.states}.}
  \item{FID}{The name of the column in the \code{spmap} to use as unique
    \dQuote{feature IDs}, corresponding to the left-hand-side of
    \code{formula}. In the included \link{spmap.states} object, the
    column with two-letter state abbreviations is called \sQuote{STATE}.
}
  \item{exclude}{A vector of feature IDs to exclude from the
    plot. \emph{Note:} any rows not present in the \code{mrp} data
    itself will automatically be excluded. Cells with \code{NA} are
    filled in the color given by \sQuote{reference.line$col}, which
    in our theme defaults to a 68\%
    opaque black, or \code{"#00000044"}.}
  \item{add.settings}{A list of lattice graphical parameters, suitable
    for \code{trellis.par.set()}. These will override the MRP defaults,
    which bear mention for two reasons: 1) top and left strip titles are
    drawn only in panels on the top and left of the full lattice. 2) any
    \code{NA}-valued parts of any panel will be filled in the color given
  by \sQuote{reference.line$col}, which in our theme defaults to a 68\%
opaque black, or \code{"#00000044"}.}
  \item{\dots}{further arguments to be passed to
    \code{\link[sp]{spplot}}}
  }

  \keyword{methods}
