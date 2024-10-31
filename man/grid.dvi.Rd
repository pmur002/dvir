\name{grid.dvi}
\alias{grid.dvi}
\alias{dviGrob}
\alias{dviGrob.character}
\alias{dviGrob.DVI}
\title{
  Render DVI File in R
}
\description{
  Render a DVI file in R graphics.
}
\usage{
dviGrob(dvi, \dots) 
\method{dviGrob}{character}(dvi, x = 0.5, y = 0.5,
        default.units = "npc", just = "centre", 
        rot = 0, device = names(dev.cur()), name = NULL,
        engine=latexEngine,
        initFonts=getOption("dvir.initFonts"),
        \dots)
\method{dviGrob}{DVI}(dvi, x = 0.5, y = 0.5,
        default.units = "npc", just = "centre", 
        rot = 0, device = names(dev.cur()), name = NULL, 
        engine=latexEngine,
        initFonts=getOption("dvir.initFonts"),
        \dots)

grid.dvi(\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dvi}{A \code{"DVI"} object, as produced by \code{\link{readDVI},
    or the name of a DVI file}.}
  \item{x, y}{Numeric values or units specifying where to draw the
    output.}
  \item{default.units}{Units to use if \code{x} or \code{y} are
    numeric.}
  \item{just}{Justification of the output relative to the
    \code{x}/\code{y} location.}
  \item{rot}{Angle of rotation (degrees).}
  \item{device}{Character value describing the name of the device that
    the output is to be drawn on.}
  \item{name}{Character value giving name for the grob.}
  \item{engine}{The TeX engine that was used to generate the DVI file.}
  \item{initFonts}{Whether to reset the font cache;  set to \code{FALSE}
    to allow font caching across multiple calls.}
  \item{\dots}{
    Arguments specific to methods of \code{dviGrob} or,
    for \code{grid.dvi}, arguments passed to \code{dviGrob}.
  }
}
\value{
  A \code{"DVIgrob"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{readDVI}}, \code{\link{grid.latex}}
}
\examples{
dvi <- readDVI(system.file("dvi", "test.dvi", package="dvir"))
\dontrun{
% Requires TeX installation
grid.dvi(dvi)
}
}
\keyword{ dplot }

