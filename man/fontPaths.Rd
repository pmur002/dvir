\name{fontPaths}
\alias{fontPaths}
\title{
  Generate paths to font files
}
\description{
  Generate paths to font files for embedding fonts in a PDF or
  PostScript file. 
}
\usage{
fontPaths(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A "DVIgrob" object as produced by \code{\link{grid.dvi}}
    or \code{\link{grid.dvi}}.}
}
\value{
  A character value containing font paths separated by colons.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.dvi}}, \code{\link{grid.latex}}
}
\examples{
dvi <- readDVI(system.file("dvi", "test.dvi", package="dvir"))
\dontrun{
% Requires TeX installation
dg <- dviGrob(dvi)
fontPaths(dg)
}
}
\keyword{ dplot }
