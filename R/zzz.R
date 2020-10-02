
.onLoad <- function(libname, pkgname) {
    initFontMap()
    initFontConfig()
    initLua()

    dvirPostamble <- "\\end{document}"
    
    options(dvir.preamble=paste(c("\\documentclass[12pt]{standalone}",
                                  "\\begin{document}"),
                                collapse="\n"),
            dvir.postamble=dvirPostamble,
            tikz.preamble=tikzPreamble(),
            tikzpicture.preamble=tikzpicturePreamble(),
            tikzpicture.postamble=paste(c("\\end{tikzpicture}",
                                          dvirPostamble),
                                        collapse="\n"))
}

