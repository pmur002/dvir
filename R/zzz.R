
.onLoad <- function(libname, pkgname) {
    initFontMap()
    initFontConfig()
    initLua()

    options(dvir.preamble=paste(c("\\documentclass[12pt]{standalone}",
                                  "\\begin{document}"),
                                collapse="\n"),
            dvir.postamble="\\end{document}",
            tikz.preamble=paste(c("\\documentclass[12pt]{standalone}",
                                  paste0("\\def\\pgfsysdriver{",
                                         system.file("tikz", "pgfsys-dvir.def",
                                                     package="dvir"),
                                         "}"),
                                  "\\usepackage{tikz}",
                                  "\\begin{document}"),
                                collapse="\n"))
}

