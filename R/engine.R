
engine <- function(engine, options="",
                   readFonts=readFontInfo,
                   fontDef=defineFont,
                   charEnc=getChar,
                   charMetric=charWidth) {
    engine <- list(engine=engine, options=options,
                   readFonts=readFonts, fontDef=fontDef,
                   charEnc=charEnc, charMetric=charMetric)
    class(engine) <- "DVIRengine"
    engine
}

texEngine <- engine("latex")

xetexEngine <- engine("xelatex", "-no-pdf")

luatexEngine <- engine("lualatex", "--output-format=dvi",
                       readFonts=luaReadFontInfo,
                       fontDef=luaDefineFont,
                       charEnc=luaGetChar,
                       charMetric=luaCharWidth)
