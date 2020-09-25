
engine <- function(engine, options="",
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

texEngine <- engine("latex")

xetexEngine <- engine("xelatex", "-no-pdf")

luatexEngine <- engine("lualatex", "--output-format=dvi",
                       readFonts=luaReadFontInfo,
                       fontDef=luaDefineFont,
                       charEnc=luaGetChar,
                       charMetric=luaCharWidth)
