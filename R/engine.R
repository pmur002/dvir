
engine <- function(engine, options="", fontDef=defineFont) {
    engine <- list(engine=engine, options=options, fontDef=fontDef)
    class(engine) <- "DVIRengine"
    engine
}

texEngine <- engine("latex")

xetexEngine <- engine("xelatex", "-no-pdf")

luatexEngine <- engine("lualatex", "--output-format=dvi", luaDefineFont)
