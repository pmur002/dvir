
## Build a data frame with a row of information for each glyph

glyph <- function(x, y, char, index, family, weight, style, size,
                  filename="", fontindex=0) {
    data.frame(x, y, char, index, family, weight, style, size,
               filename, fontindex)
}

addGlyph <- function(glyph) {
    glyphs <- get("glyphs")
    glyphNum <- get("glyphNum")
    glyphs[[glyphNum]] <- glyph
    set("glyphs", glyphs)
    set("glyphNum", glyphNum + 1)
}

ttxGlyphIndex <- function(raw, fontpath, device) {
    tmpFontDir <- initFontDir()
    filename <- basename(fontpath)
    fontsuffix <- "[.](ttf|otf)$"
    if (!grepl(fontsuffix, filename))
        warning("Unrecognised font suffix")
    filesuffix <- gsub(paste0(".+", fontsuffix), "\\1", filename)
    filestub <- gsub(fontsuffix, "", filename)
    fontfile <- file.path(tmpFontDir, filename)
    if (!file.exists(fontfile)) {
        file.copy(fontpath, tmpFontDir)
    }
    ## Convert char num to glyph name
    glyphName <- getGlyphName(raw, device, fontfile, filesuffix)
    ## Find glyph index
    glyphs <- getGlyphs(fontfile, filesuffix)
    xml_text(xml_find_first(glyphs,
                            paste0("//GlyphID[@name = '", glyphName, "']/@id")))
}

glyphIndex <- function(raw, fonts, f) {
    font <- fonts[[f]]
    if (font$TeXfont) {
        warning("TeX fonts not (yet) supported")
        NA
    } else {
        device <- get("device")
        ## Calculate glyph index from TTX glyphs
        ttxGlyphIndex(raw, font$file, device)
    }
}

fontWeight <- function(fonts, f) {
    font <- getFontFile(fonts[[f]]$file)
    OS2 <- getOS2(font$file, font$suffix)
    weight <- xml_text(xml_find_first(OS2, "//usWeightClass/@value"))
    if (is.na(weight))
        400
    else
        as.numeric(weight)
}

fontStyle <- function(fonts, f) {
    font <- getFontFile(fonts[[f]]$file)
    hhea <- getHHea(font$file, font$suffix)
    slopeRun <- xml_text(xml_find_first(hhea, "//caretSlopeRun/@value"))
    if (is.na(slopeRun) || slopeRun == "0")
        "normal"
    else
        "italic"
}

## set_char_<i>
## Similar to op_set_char(), but do not generate a grob
glyph_set_char <- function(op) {
    x <- fromTeX(get("h") - get("left"))
    y <- fromTeX(get("v") - get("top"))
    fonts <- get("fonts")
    f <- get("f")
    if (fonts[[f]]$TeXfont)
        stop("Glyph support not available for TeX/Type1 fonts")
    device <- get("device")
    engine <- get("engine")
    char <- engine$charEnc(op$blocks$op.opcode$fileRaw,
                           fonts[[f]]$postscriptname,
                           device)
    family <- fontFamily(fonts[[f]], char, device)

    index <- glyphIndex(op$blocks$op.opcode$fileRaw, fonts, f)
    ## These need to be cached as part of the font
    weight <- fontWeight(fonts, f)
    style <- fontStyle(fonts, f)
    addGlyph(glyph(x, y, char, index, family, weight, style,
                   fonts[[f]]$size, fonts[[f]]$file))
    
    set("h",
        get("h") + engine$charMetric(op$blocks$op.opcode$fileRaw, fonts, f))    
}
for (i in 0:127) {
    assign(paste0("glyph_op_", i), glyph_set_char)
}

## set_char_<i>
## Similar to op_set(), but do not generate a grob
glyph_set <- function(op) {
    x <- fromTeX(get("h") - get("left"))
    y <- fromTeX(get("v") - get("top"))
    fonts <- get("fonts")
    f <- get("f")
    if (fonts[[f]]$TeXfont)
        stop("Glyph support not available for TeX/Type1 fonts")
    device <- get("device")
    engine <- get("engine")
    char <- engine$charEnc(op$blocks$op.opparams$fileRaw,
                           fonts[[f]]$postscriptname,
                           device)
    family <- fontFamily(fonts[[f]], char, device)

    index <- glyphIndex(op$blocks$op.opcode$fileRaw, fonts, f)
    ## These need to be cached as part of the font
    weight <- fontWeight(fonts, f)
    style <- fontStyle(fonts, f)
    addGlyph(glyph(x, y, char, index, family, weight, style,
                   fonts[[f]]$size, fonts[[f]]$file))
    
    set("h",
        get("h") + engine$charMetric(op$blocks$op.opparams$fileRaw, fonts, f))
}
glyph_op_128 <- glyph_set
glyph_op_129 <- glyph_set
glyph_op_130 <- glyph_set
glyph_op_131 <- glyph_set

## set_rule
glyph_op_132 <- op_set_rule

## put_rule
glyph_op_137 <- op_put_rule

## bop
glyph_op_139 <- op_bop

## eop
glyph_op_140 <- op_ignore

## push
glyph_op_141 <- op_push

## pop
glyph_op_142 <- op_pop

## right<i>
glyph_op_143 <- op_right
glyph_op_144 <- op_right
glyph_op_145 <- op_right
glyph_op_146 <- op_right

## w<i>
glyph_op_147 <- op_w
glyph_op_148 <- op_w
glyph_op_149 <- op_w
glyph_op_150 <- op_w
glyph_op_151 <- op_w

## x<i>
glyph_op_152 <- op_x
glyph_op_153 <- op_x
glyph_op_154 <- op_x
glyph_op_155 <- op_x
glyph_op_156 <- op_x

## down<i>
glyph_op_157 <- op_down
glyph_op_158 <- op_down
glyph_op_159 <- op_down
glyph_op_160 <- op_down

## y<i>
glyph_op_161 <- op_y
glyph_op_162 <- op_y
glyph_op_163 <- op_y
glyph_op_164 <- op_y
glyph_op_165 <- op_y

## z<i>
glyph_op_166 <- op_z
glyph_op_167 <- op_z
glyph_op_168 <- op_z
glyph_op_169 <- op_z
glyph_op_170 <- op_z

## fnt_num_<i>
for (i in 171:234) {
    assign(paste0("glyph_op_", i), op_fnt_num)
}

## xxx<i> (specials)
glyph_op_239 <- op_ignore
glyph_op_240 <- op_ignore
glyph_op_241 <- op_ignore
glyph_op_242 <- op_ignore

## font_def<i>
glyph_op_243 <- op_ignore

## pre
glyph_op_247 <- op_ignore

## post
glyph_op_248 <- op_ignore

## post_post
glyph_op_249 <- op_ignore

op2glyph <- function(op) {
    opcode <- blockValue(op$blocks$op.opcode)
    base::get(paste0("glyph_op_", opcode))(op)
}

dvi2glyphs <- function(dvi, metrics, device, engine) {
    set("device", device)
    set("engine", engine)

    set("glyphs", vector("list", length(dvi)))
    set("glyphNum", 1)
    invisible(lapply(dvi, op2glyph))
    glyphs <- do.call(rbind, get("glyphs"))
    width <- abs(metrics$right - metrics$left)
    height <- abs(metrics$top - metrics$bottom)
    right <- convertX(unit(width, "mm"), "bigpts", valueOnly=TRUE)
    top <- convertHeight(unit(height, "mm"), "bigpts", valueOnly=TRUE)
    glyphInfo(glyphs$index,
              convertX(unit(glyphs$x, "mm"), "bigpts", valueOnly=TRUE),
              convertY(unit(height - glyphs$y, "mm"), "bigpts", valueOnly=TRUE),
              glyphs$family,
              glyphs$weight,
              glyphs$style,
              glyphs$size,
              glyphs$filename,
              glyphs$fontindex,
              convertWidth(unit(width, "mm"),
                           "bigpts", valueOnly=TRUE),
              convertHeight(unit(height, "mm"),
                            "bigpts", valueOnly=TRUE),
              c(left=0, right=right, centre=right/2),
              
              c(bottom=0, top=top, centre=top/2,
                baseline=convertHeight(unit(abs(metrics$bottom -
                                                metrics$baseline),
                                            "mm"), "bigpts", valueOnly=TRUE)))
}


################################################################################
## User interface
dviGlyph <- function(dvi, ...) {
    UseMethod("dviGlyph")
}

dviGlyph.character <- function(dvi, ...) {
    dviGlyph(readDVI(dvi), ...)
}

dviGlyph.DVI <- function(dvi,
                         x=.5, y=.5, default.units="npc",
                         device=names(dev.cur()),
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
    
    dvi2glyphs(dvi, metrics, device, engine)
}

