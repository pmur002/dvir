\name{typeset}
\alias{typeset}
\title{
  Generate DVI File from TeX Code
}
\description{
  Generate a DVI File from TeX Code and return the name of the DVI file.
}
\usage{
typeset(tex, 
        preamble=getOption("dvir.preamble"),
        postamble=getOption("dvir.postamble"),
        engine=latexEngine, tinytex=FALSE, file=NULL, quiet=TRUE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tex}{A character value containing LaTeX code.}
  \item{preamble,postamble}{LaTeX code that is placed around \code{tex}
    before processing.}
  \item{engine}{The TeX engine that is used for processing the LaTeX
    code.}
  \item{tinytex}{Should \pkg{dvir} attempt to use \pkg{tinytex} to
    perform the processing?}
  \item{file}{The name of a file to read LaTeX code from. If \code{tex}
    is missing, this file is read.}
  \item{quiet}{Suppress TeX engine output?}
}
\details{
  This function takes TeX code as input, creates a TeX document, and 
  processes the TeX document
  to produce a DVI file.
}
\value{
  The name of the DVI file.
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
typeset("$x - \\\\mu$")
}
}
\keyword{ dplot }
