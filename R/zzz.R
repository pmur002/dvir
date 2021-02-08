
dvirPreamble <- paste(c("\\documentclass[12pt]{standalone}",
                        "\\begin{document}"),
                      collapse="\n")

dvirPostamble <- "\\end{document}"

.onLoad <- function(libname, pkgname) {
    initFontMap()
    initFontConfig()
    initLua()

    options(dvir.preamble=dvirPreamble,
            dvir.postamble=dvirPostamble,
            dvir.initFonts=TRUE)
}

