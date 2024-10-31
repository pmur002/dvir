\name{luaEngine}
\alias{luaEngine}
\alias{lualatexEngine}
\title{
  LuaTeX engines
}
\description{
  These objects represent different LuaTeX engines that can
  be selected for processing LaTeX code to produce DVI output.
}
\usage{
luaEngine(engine="lualatex",
          options="--output-format=dvi",
          readFonts=luaReadFontInfo,
          fontDef=luaDefineFont,
          charEnc=luaGetChar,
          charMetric=luaCharWidth,
          special=noSpecial) 
lualatexEngine
}
%- maybe also 'usage' for other objects documented here.
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
  \code{\link{grid.lualatex}}
}
\keyword{ dplot }
