\name{grid.tikz}
\alias{grid.tikz}
\alias{grid.tikzpicture}
\alias{tikzGrob}
\alias{tikzpictureGrob}
\alias{tikzPreamble}
\alias{tikzpicturePreamble}
\alias{tikzpicturePostamble}
\title{
  Render a TikZ picture in R
}
\description{
  Render a TikZ picture in R graphics.
}
\usage{
tikzpictureGrob(tex, ...,
                bbox=NULL,
                preamble=tikzpicturePreamble(),
                postamble=tikzpicturePostamble(bbox),
                engine=TeXengine("latex", special=tikzSpecial))
grid.tikzpicture(...)
tikzGrob(tex, ...,
         preamble=tikzPreamble(),
         postamble=getOption("dvir.postamble"),
         engine=TeXengine("latex", special=tikzSpecial))
grid.tikz(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tex}{A character value containing LaTeX code.}
  \item{bbox}{A bounding box to enforce for the picture.}
  \item{preamble,postamble}{LaTeX code that is placed around \code{tex}
    before processing.}
  \item{engine}{The TeX engine that is used for processing the LaTeX
    code.}
  \item{\dots}{
    Arguments passed to \code{latexGrob}.
  }
}
\details{
  The \code{grid.tikzpicture}
  function takes TikZ code as input, processes the TikZ code
  to produce a DVI file, reads the DVI file, and renders the result
  using \pkg{grid}.

  The default preamble loads the TikZ package and starts a TikZ
  picture environment (so \code{tex} only needs to contain TikZ
  code). The \code{tikzPreamble} function can be used
  create variations on that.

  The default postamble closes the TikZ picture environment.
  If \code{bbox} has been specified, additional TikZ and PGF
  code is added to enforce the bounding box.
  The \code{tikzPostamble} function can be used to create
  variations on that.

  The default engine is a LaTeX engine, with \code{tikzSpecial} as
  to handle the DVI specials that are generated from the TikZ code.

  The \code{grid.tikz}  function loads the TikZ package, but does
  not start a TikZ picture environment.  This allows for a combination
  of normal LaTeX content along with a TikZ picture.
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
