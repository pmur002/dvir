
TeXengine <- function(engine, options="",
                      readFonts=readFontInfo,
                      fontDef=defineFont,
                      charEnc=getChar,
                      charMetric=charWidth,
                      special=noSpecial,
                      dviSuffix=".dvi") {
    engine <- list(engine=engine, options=options,
                   readFonts=readFonts, fontDef=fontDef,
                   charEnc=charEnc, charMetric=charMetric,
                   special=special, dviSuffix=dviSuffix)
    class(engine) <- "DVIRengine"
    engine
}

latexEngine <- TeXengine("latex")


