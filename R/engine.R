
TeXengine <- function(engine, options="",
                   readFonts=readFontInfo,
                   fontDef=defineFont,
                   charEnc=getChar,
                   charMetric=charWidth,
                   special=noSpecial) {
    engine <- list(engine=engine, options=options,
                   readFonts=readFonts, fontDef=fontDef,
                   charEnc=charEnc, charMetric=charMetric,
                   special=special)
    class(engine) <- "DVIRengine"
    engine
}

latexEngine <- TeXengine("latex")

xetexEngine <- TeXengine("xelatex", "-no-pdf")

