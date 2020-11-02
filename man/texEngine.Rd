\name{latexEngine}
\alias{TeXengine}
\alias{latexEngine}
\alias{luatexEngine}
\title{
  TeX Engines
}
\description{
  These objects represent different TeX engines that can
  be selected for processing LaTeX code to produce DVI output.
}
\usage{
latexEngine
luatexEngine
TeXengine(engine, options="",
          readFonts=readFontInfo,
          fontDef=defineFont,
          charEnc=getChar,
          charMetric=charWidth,
          special=noSpecial)
}
\arguments{
\item{engine}{The command to run to process LaTeX documents.}
\item{options}{Options to the \code{engine} command.}
\item{readFonts}{
  A function to read font information
  within a DVI file.
}
\item{fontDef}{
  A function to generate font definitions from font information.
}
\item{charEnc}{
  A function to produce a character value from a DVI char operation.
}
\item{charMetric}{
  A function to calculate the width of a character.
}
\item{special}{
  A "handler" for special operations within a DVI file;
  see \code{\link{noSpecial}} and \code{\link{tikzSpecial}}.
}
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.latex}}
}
\keyword{ dplot }
