\name{panel.polygonsplot}
\alias{panel.polygonsplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
panel function to plot NA-valued polygons in alternate color.}
\description{
Except for the color used to plot \code{NA} values, this is identical to
the panel function used by \code{\link[sp]{spplot}}.}
\usage{
panel.polygonsplot(x, y, z, subscripts, at = pretty(z), shrink, labels = NULL, label.style = c("mixed", "flat", "align"), contour = FALSE, region = TRUE, col = add.line$col, lty = add.line$lty, lwd = add.line$lwd, cex = add.text$cex, font = add.text$font, fontfamily = add.text$fontfamily, fontface = add.text$fontface, col.text = add.text$col, ..., col.regions = regions$col, alpha.regions = regions$alpha, grid.polygons, sp.layout, emptycolor = "#00000033")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{subscripts}{
%%     ~~Describe \code{subscripts} here~~
}
  \item{at}{
%%     ~~Describe \code{at} here~~
}
  \item{shrink}{
%%     ~~Describe \code{shrink} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{label.style}{
%%     ~~Describe \code{label.style} here~~
}
  \item{contour}{
%%     ~~Describe \code{contour} here~~
}
  \item{region}{
%%     ~~Describe \code{region} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{font}{
%%     ~~Describe \code{font} here~~
}
  \item{fontfamily}{
%%     ~~Describe \code{fontfamily} here~~
}
  \item{fontface}{
%%     ~~Describe \code{fontface} here~~
}
  \item{col.text}{
%%     ~~Describe \code{col.text} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{col.regions}{
%%     ~~Describe \code{col.regions} here~~
}
  \item{alpha.regions}{
%%     ~~Describe \code{alpha.regions} here~~
}
  \item{grid.polygons}{
%%     ~~Describe \code{grid.polygons} here~~
}
  \item{sp.layout}{
%%     ~~Describe \code{sp.layout} here~~
}
  \item{emptycolor}{
%%     ~~Describe \code{emptycolor} here~~
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

\seealso{
\code{\link[sp]{spplot}}
}
\keyword{ graphics }