
## ONLY gather typeset characters
## (NO hlines or vlines or TikZ etc)

## Build a data frame with a row of information for each glyph

## set_char_<i>
## Similar to op_set_char(), but do not generate a grob
typesetsetchar <- function(op) {
    x <- fromTeX(get("h") - get("left"))
    y <- fromTeX(get("v") - get("top"))
    fonts <- get("fonts")
    f <- get("f")
    if (fonts[[f]]$TeXfont)
        stop("Glyph support not available for TeX/Type1 fonts")
    device <- get("device")
    engine <- get("engine")
    colour <- get("colour")
    char <- engine$charEnc(op$blocks$op.opcode$fileRaw,
                           fonts[[f]]$postscriptname,
                           device)
    family <- fontFamily(fonts[[f]], char, device)

    index <- glyphIndex(op$blocks$op.opcode$fileRaw, fonts, f)
    ## These need to be cached as part of the font
    weight <- fontWeight(fonts, f)
    style <- fontStyle(fonts, f)
    addGlyph(glyph(x, y, char, index, family, weight, style,
                   fonts[[f]]$size, fonts[[f]]$file, colour[1]))
    
    set("h",
        get("h") + engine$charMetric(op$blocks$op.opcode$fileRaw, fonts, f))    
}
## Extra function call layer to ease debugging
typeset_set_char <- function(op) typesetsetchar(op)
for (i in 0:127) {
    assign(paste0("typeset_op_", i), typeset_set_char)
}

## set_char_<i>
## Similar to op_set(), but do not generate a grob
typesetset <- function(op) {
    x <- fromTeX(get("h") - get("left"))
    y <- fromTeX(get("v") - get("top"))
    fonts <- get("fonts")
    f <- get("f")
    if (fonts[[f]]$TeXfont)
        stop("Glyph support not available for TeX/Type1 fonts")
    device <- get("device")
    engine <- get("engine")
    colour <- get("colour")
    char <- engine$charEnc(op$blocks$op.opparams$fileRaw,
                           fonts[[f]]$postscriptname,
                           device)
    family <- fontFamily(fonts[[f]], char, device)

    index <- glyphIndex(op$blocks$op.opcode$fileRaw, fonts, f)
    ## These need to be cached as part of the font
    weight <- fontWeight(fonts, f)
    style <- fontStyle(fonts, f)
    addGlyph(glyph(x, y, char, index, family, weight, style,
                   fonts[[f]]$size, fonts[[f]]$file, colour[1]))
    
    set("h",
        get("h") + engine$charMetric(op$blocks$op.opparams$fileRaw, fonts, f))
}
typeset_set <- function(op) typesetset(op)
typeset_op_128 <- typeset_set
typeset_op_129 <- typeset_set
typeset_op_130 <- typeset_set
typeset_op_131 <- typeset_set

## set_rule
typeset_op_132 <- op_set_rule

## put_rule
typeset_op_137 <- op_put_rule

## bop
typeset_op_139 <- op_bop

## eop
typeset_op_140 <- op_ignore

## push
typeset_op_141 <- op_push

## pop
typeset_op_142 <- op_pop

## right<i>
typeset_op_143 <- op_right
typeset_op_144 <- op_right
typeset_op_145 <- op_right
typeset_op_146 <- op_right

## w<i>
typeset_op_147 <- op_w
typeset_op_148 <- op_w
typeset_op_149 <- op_w
typeset_op_150 <- op_w
typeset_op_151 <- op_w

## x<i>
typeset_op_152 <- op_x
typeset_op_153 <- op_x
typeset_op_154 <- op_x
typeset_op_155 <- op_x
typeset_op_156 <- op_x

## down<i>
typeset_op_157 <- op_down
typeset_op_158 <- op_down
typeset_op_159 <- op_down
typeset_op_160 <- op_down

## y<i>
typeset_op_161 <- op_y
typeset_op_162 <- op_y
typeset_op_163 <- op_y
typeset_op_164 <- op_y
typeset_op_165 <- op_y

## z<i>
typeset_op_166 <- op_z
typeset_op_167 <- op_z
typeset_op_168 <- op_z
typeset_op_169 <- op_z
typeset_op_170 <- op_z

## fnt_num_<i>
for (i in 171:234) {
    assign(paste0("typeset_op_", i), op_fnt_num)
}

## xxx<i> (specials)
typesetSpecial <- function(op) {
    engine <- get("engine")
    engine$special$grob(op)
}

typeset_op_239 <- typesetSpecial
typeset_op_240 <- typesetSpecial
typeset_op_241 <- typesetSpecial
typeset_op_242 <- typesetSpecial

## font_def<i>
typeset_op_243 <- op_ignore

## pre
typeset_op_247 <- op_ignore

## post
typeset_op_248 <- op_ignore

## post_post
typeset_op_249 <- op_ignore

op2typeset <- function(op) {
    opcode <- blockValue(op$blocks$op.opcode)
    base::get(paste0("typeset_op_", opcode))(op)
}

dvitypeset <- function(dvi, metrics, device, engine) {
    set("device", device)
    set("engine", engine)

    set("glyphs", vector("list", length(dvi)))
    set("glyphNum", 1)
    invisible(lapply(dvi, op2typeset))
    glyphs <- do.call(rbind, get("glyphs"))
    width <- abs(metrics$right - metrics$left)
    height <- abs(metrics$top - metrics$bottom)
    right <- convertX(unit(width, "mm"), "bigpts", valueOnly=TRUE)
    top <- convertHeight(unit(height, "mm"), "bigpts", valueOnly=TRUE)
    if (is.finite(metrics$baseline)) {
        vAnchor <- glyphAnchor(c(0, top, top/2,
                                 convertHeight(unit(metrics$baseline, "mm"),
                                               "bigpts", valueOnly=TRUE)),
                               label=c("bottom", "top", "centre", "baseline"))
    } else {
        vAnchor <- glyphAnchor(c(0, top, top/2),
                               label=c("bottom", "top", "centre"))
    }
    glyphInfo(glyphs$index,
              convertX(unit(glyphs$x, "mm"), "bigpts", valueOnly=TRUE),
              convertY(unit(height - glyphs$y, "mm"), "bigpts", valueOnly=TRUE),
              glyphs$family,
              glyphs$weight,
              glyphs$style,
              glyphs$size,
              glyphs$filename,
              glyphs$fontindex,
              glyphWidth(convertWidth(unit(width, "mm"),
                                      "bigpts", valueOnly=TRUE)),
              glyphHeight(convertHeight(unit(height, "mm"),
                                        "bigpts", valueOnly=TRUE)),
              hAnchor=glyphAnchor(c(0, right, right/2),
                                  label=c("left", "right", "centre")),
              vAnchor=vAnchor,
              glyphs$colour)
}

dviTypeset <- function(dvi,
                       x=0.5, y=0.5,
                       default.units="npc",
                       hjust="centre", vjust="centre",
                       device=names(dev.cur()),
                       name=NULL,
                       engine=latexEngine,
                       initFonts=getOption("dvir.initFonts"),
                       ...) {
    set("initFonts", initFonts)
    set("scale", 1)

    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (device == "null device") {
        dev.new()
        device <- names(dev.cur())
    }
    fonts <- dviFonts(dvi, device, engine)
    metrics <- dviMetric(dvi, device, engine)
    glyphs <- dvitypeset(dvi, metrics, device, engine)
    ## Ensure that metric PDF device is killed
    killMetricDev()
    glyphGrob(glyphs, x, y, hjust=hjust, vjust=vjust, name=name)
}

################################################################################
## User interface

typesetGrob <- function(tex,
                        x=0.5, y=0.5,
                        default.units="npc",
                        hjust="centre", vjust="centre",
                        device=names(dev.cur()),
                        name=NULL,
                        preamble=getOption("dvir.preamble"),
                        postamble=getOption("dvir.postamble"),
                        engine=latexEngine,
                        file=NULL,
                        initFonts=getOption("dvir.initFonts"),
                        quiet=TRUE) {
    if (missing(tex)) {
        if (is.null(file))
            stop("Must specify one of 'tex' or 'file'")
        tex <- readLines(file)
    }
    dviFile <- typeset(tex, preamble, postamble, engine, tinytex=FALSE,
                       quiet=quiet)
    dvi <- readDVI(dviFile)
    dviTypeset(dvi, x, y, default.units, hjust, vjust, device,
               name, engine, initFonts)
}

grid.typeset <- function(...) {
    grid.draw(typesetGrob(...))
}

grid.latexTypeset <- function(...) {
    grid.typeset(...)
}

grid.xelatexTypeset <- function(tex, ...,
                                preamble=xePreamble(),
                                postamble=xePostamble(),
                                engine=xelatexEngine) {
    grid.typeset(tex, ...,
                 preamble=preamble, postamble=postamble, engine=engine)
}

grid.lualatexTypeset <- function(tex, ...,
                                 preamble=luaPreamble(),
                                 postamble=getOption("dvir.postamble"),
                                 engine=lualatexEngine) {
    grid.typeset(tex, ...,
                 preamble=preamble, postamble=postamble, engine=engine)    
}

