\name{NEWS}
\title{NEWS file for the dvir package}
\encoding{UTF-8}

\section{Changes in version 0.4-0}{
  \itemize{
    \item Add \code{glyph} argument to create
    \code{glyphGrob()} rather than \code{textGrob()}
    for each glyph in DVI file.

    \item Fix definition of \code{dvir:::get()} and use of \code{base::get()}.
  }
}

\section{Changes in version 0.3-3}{
  \itemize{
    \item Add \code{quiet} argument to \code{typeset()} and
    \code{grid.latex()} to suppress TeX engine output.
  }
}

\section{Changes in version 0.3-2}{
  \itemize{
    \item Add \code{initFonts} argument to \code{grid.latex()} et al.,
    which by default is \code{FALSE} (settable by
    \code{options("initFonts")}), which means that font information
    read from DVI files is retained across calls to \code{grid.latex()},
    which results in speed improvement (when the same fonts are reused).
  }
}

\section{Changes in version 0.3-1}{
  \itemize{
    \item Add support for Open Type fonts (in TeX documents) on
    Cairo-based devices.  Makes use of \pkg{systemfonts} package.

    \item Add support for \code{set3} operations (in DVI files),
    e.g., math glyphs in a UNICODE font.
    
    \item New function \code{typeset()} so users can easily perform the
    generate-tex-doc-and-process-to-dvi-file step (separate from reading
    DVI file, which is \code{readDVI()}, and separate from rendering, which is
    \code{grid.dvi()}).

    \item Bug fix to ensure that the temporary PDF device that is opened
    for font metric calculations gets closed again.
  }
}

\section{Changes in version 0.3-0}{
  \itemize{
    \item Add support for TikZ graphics.
  }
}

\section{Changes in version 0.2-2}{
  \itemize{
    \item Speed up by removing redundant font_def handling in "metric"
    and "grid" sweeps of DVI file (just reuse font info from "font" sweep).
  }
}

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
