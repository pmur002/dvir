
## Build a data frame with a row of information for each glyph

glyph <- function(x, y, char, index, family, weight, style, size) {
    left <- fromTeX(get("left"))
    top <- fromTeX(get("top"))
    data.frame(x=x - left, y=y - top, char, index, family, weight, style, size)
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

ttxFontWeight <- function(fontpath) {
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
    ttxfile <- file.path(tmpFontDir, paste0(filestub, "-OS2.ttx"))
    if (!file.exists(ttxfile)) {
        system(paste0("ttx -t OS/2 -o ", ttxfile, " ", fontfile))
    }
    OS2 <- getTTX(ttxfile)
    weight <- xml_text(xml_find_first(OS2, "//usWeightClass/@value"))
    if (is.na(weight))
        400
    else
        as.numeric(weight)
}

fontWeight <- function(fonts, f) {
    font <- fonts[[f]]
    ttxFontWeight(font$file)
}

ttxFontStyle <- function(fontpath) {
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
    ttxfile <- file.path(tmpFontDir, paste0(filestub, "-hhea.ttx"))
    if (!file.exists(ttxfile)) {
        system(paste0("ttx -t hhea -o ", ttxfile, " ", fontfile))
    }
    hhea <- getTTX(ttxfile)
    slopeRun <- xml_text(xml_find_first(hhea, "//caretSlopeRun/@value"))
    if (is.na(slopeRun) || slopeRun == "0")
        "normal"
    else
        "italic"
}

fontStyle <- function(fonts, f) {
    font <- fonts[[f]]
    ttxFontStyle(font$file)
}

## set_char_<i>
## Similar to op_set_char(), but do not generate a grob
glyph_set_char <- function(op) {
    x <- fromTeX(get("h"))
    y <- fromTeX(get("v"))
    fonts <- get("fonts")
    f <- get("f")
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
                   size=fonts[[f]]$size))
    
    set("h",
        get("h") + engine$charMetric(op$blocks$op.opcode$fileRaw, fonts, f))    
}
for (i in 0:127) {
    assign(paste0("glyph_op_", i), glyph_set_char)
}

## set_char_<i>
## Similar to op_set(), but do not generate a grob
glyph_set <- function(op) {
    x <- fromTeX(get("h"))
    y <- fromTeX(get("v"))
    fonts <- get("fonts")
    f <- get("f")
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
                   size=fonts[[f]]$size))
    
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

dvi2glyphs <- function(dvi, device, engine) {
    set("device", device)
    set("engine", engine)
    set("scale", 1)

    set("glyphs", vector("list", length(dvi)))
    set("glyphNum", 1)
    invisible(lapply(dvi, op2glyph))
    do.call(rbind, get("glyphs"))
}

################################################################################
## User interface
dviGlyphs <- function(dvi, ...) {
    UseMethod("dviGlyphs")
}

dviGlyphs.character <- function(dvi, ...) {
    dviGlyphs(readDVI(dvi), ...)
}

dviGlyphs.DVI <- function(dvi, 
                          device=names(dev.cur()),
                          engine=latexEngine,
                          initFonts=getOption("dvir.initFonts"),
                          ...) {
    set("initFonts", initFonts)
    fonts <- dviFonts(dvi, device, engine)
    metrics <- dviMetric(dvi, device, engine)
    dvi2glyphs(dvi, device, engine)
}
