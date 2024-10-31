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
          rot = 0, device = names(dev.cur()), name = NULL,
          preamble=getOption("dvir.preamble"),
          postamble=getOption("dvir.postamble"),
          engine=latexEngine, tinytex=FALSE, file=NULL,
          initFonts=getOption("dvir.initFonts"),
          quiet=TRUE) 
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
  \item{preamble,postamble}{LaTeX code that is placed around \code{tex}
    before processing.}
  \item{engine}{The TeX engine that is used for processing the LaTeX
    code.}
  \item{tinytex}{Should \pkg{dvir} attempt to use \pkg{tinytex} to
    perform the processing?}
  \item{file}{The name of a file to read LaTeX code from. If \code{tex}
    is missing, this file is read.}
  \item{initFonts}{Whether to reset the font cache;  set to \code{FALSE}
    to allow font caching across multiple calls.}
  \item{quiet}{Suppress TeX engine output?}
  \item{\dots}{
    Arguments passed to \code{latexGrob}.
  }
}
\details{
  This function takes LaTeX code as input, processes the LaTeX code
  to produce a DVI file, reads the DVI file, and renders the result
  using \pkg{grid}.

  The default engine is the \code{\link{latexEngine}}, which runs
  \code{latex} to generate DVI;  an alternative
  is the \code{\link{luatexEngine}}, which runs \code{dvilualatex}
  to generate DVI.
}
\value{
  A \code{"DVIgrob"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{readDVI}}, \code{\link{grid.dvi}}
}
\examples{
\dontrun{
% Requires TeX installation
grid.latex("$x - \\\\mu$")
}
}
\keyword{ dplot }
