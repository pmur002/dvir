\name{grid.lualatex}
\alias{grid.lualatex}
\alias{lualatexGrob}
\alias{luaPreamble}
\title{
  Render LaTeX in R with LuaTeX engine
}
\description{
  Render LaTeX code in R graphics using LuaTeX engine.
}
\usage{
lualatexGrob(tex, ...,
             preamble=luaPreamble(),
             postamble=getOption("dvir.postamble"),
             engine=lualatexEngine)
grid.lualatex(...)
luaPreamble(font="Times")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tex}{A character value containing LaTeX code.}
  \item{preamble,postamble}{LaTeX code that is placed around \code{tex}
    before processing.}
  \item{engine}{The TeX engine that is used for processing the LaTeX
    code.}
  \item{\dots}{
    Arguments passed to \code{latexGrob}.
  }
  \item{font}{The default font.}
}
\details{
  This function takes LaTeX code as input, processes the LaTeX code
  to produce a DVI file, reads the DVI file, and renders the result
  using \pkg{grid}.

  The default preamble does some setup appropriate to a standard
  LuaTeX document.  The \code{luaPreamble} function can be used
  create variations on that.

  The default engine is the \code{\link{lualatexEngine}}, which runs
  \code{lualatex} to generate DVI.  
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
