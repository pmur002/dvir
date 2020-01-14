
engine <- function(engine, options="") {
    engine <- list(engine=engine, options=options)
    class(engine) <- "DVIRengine"
    engine
}

latexEngine <- engine("latex")

xetexEngine <- engine("xelatex", "-no-pdf")

luatexEngine <- engine("lualatex", "--output-format=dvi")
