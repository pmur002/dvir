
engine <- function(engine, options="",
                   fontDef=defineFont,
                   charEnc=getChar,
                   charMetric=charWidth) {
    engine <- list(engine=engine, options=options,
                   fontDef=fontDef, charEnc=charEnc, charMetric=charMetric)
    class(engine) <- "DVIRengine"
    engine
}

texEngine <- engine("latex")

xetexEngine <- engine("xelatex", "-no-pdf")

luatexEngine <- engine("lualatex", "--output-format=dvi",
                       fontDef=luaDefineFont,
                       charEnc=luaGetChar,
                       charMetric=luaCharWidth)
