\name{readDVI}
\alias{readDVI}
\title{
  Read DVI File
}
\description{
  Read a DVI file (produced by LaTeX) into R. 
}
\usage{
readDVI(f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{A character value giving the name of a DVI file.}
}
\value{
  A \code{"DVI"} object.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.dvi}}, \code{\link{grid.latex}}
}
\examples{
readDVI(system.file("dvi", "test.dvi", package="dvir"))
}
\keyword{ dplot }
