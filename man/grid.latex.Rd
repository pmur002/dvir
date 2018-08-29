\name{grid.latex}
\alias{grid.latex}
\alias{latexGrob}
\title{
  Render LaTeX in R
}
\description{
  Render LaTeX code in R graphics.
}
\usage{
latexGrob(tex, x = 0.5, y = 0.5, default.units = "npc", just = "centre", 
          rot = 0, device = names(dev.cur()), name = NULL) 

grid.latex(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tex}{A character value containing LaTeX code.}
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
  \item{\dots}{
    Arguments passed to \code{latexGrob}.
  }
}
\details{
  This function takes LaTeX code as input, processes the LaTeX code
  to produce a DVI file, reads the DVI file, and renders the result
  using \pkg{grid}.
}
\value{
  A \code{"DVIgrob"} object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{readDVI}}, \code{\link{grid.dvi}}
}
\examples{
grid.latex("$x - \\\\mu$")
}
\keyword{ dplot }
