
.onLoad <- function(libname, pkgname) {
    initFontMap()
    initFontConfig()
    initLuaOTFcache()
    initTTXcache()

    options(dvir.preamble=paste(c("\\documentclass[12pt]{standalone}",
                                  "\\begin{document}"),
                                collapse="\n"),
            dvir.postamble="\\end{document}")
}

