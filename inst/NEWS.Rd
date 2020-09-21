\name{NEWS}
\title{NEWS file for the dvir package}
\encoding{UTF-8}

\section{Changes in version 0.2-1}{
  \itemize{
    \item Add FontConfig files into more than one directory to be
    compatible with newer FontConfig versions.

    \item Add support for reading (and printing) TeX specials within DVI
    files. 
  }
}

\section{Changes in version 0.2-0}{
  \itemize{
    \item Added support for LuaTeX engine.
  }
}

\section{Changes in version 0.1-2}{
  \itemize{
    \item Fix generation of .fonts.conf file to use \code{compare}
    attribute rather than \code{mode} attribute (to kill Fontconfig
    warnings).
  }
}
