
.onLoad <- function(libname, pkgname) {
    initFontMap()
    initFontConfig()
    initLua()

    tikzPreamble <- paste(c("\\documentclass[12pt]{standalone}",
                            paste0("\\def\\pgfsysdriver{",
                                   system.file("tikz", "pgfsys-dvir.def",
                                               package="dvir"),
                                   "}"),
                            "\\usepackage{tikz}",
                            "\\begin{document}"),
                          collapse="\n")
    dvirPostamble <- "\\end{document}"
    
    options(dvir.preamble=paste(c("\\documentclass[12pt]{standalone}",
                                  "\\begin{document}"),
                                collapse="\n"),
            dvir.postamble=dvirPostamble,
            tikz.preamble=tikzPreamble,
            tikzpicture.preamble=paste(c(tikzPreamble,
                                         "\\begin{tikzpicture}"),
                                       collapse="\n"),
            tikzpicture.postamble=paste(c("\\end{tikzpicture}",
                                          dvirPostamble),
                                        collapse="\n"))
}

