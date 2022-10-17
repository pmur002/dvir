
## Support for XeTeX's XDV format

################################################################################
## Reading

## XFontDef 
fontInfoMarker <- mixedBlock(flags=uint2,
                             fontname=lengthBlock(uint1, ASCIIchar),
                             fontindex=int4)
xeReadFontInfo <- function(marker) {
    if (!blockValue(marker$flags))
        NULL
    else {
        blocks <- NULL
        flags <- marker$flags
        if (bitwAnd(0x0200, flags))
            blocks <- c(blocks, colour=int4)
        if (bitwAnd(0x1000, flags))
            blocks <- c(blocks, extend=int4)
        if (bitwAnd(0x2000, flags))
            blocks <- c(blocks, slant=int4)
        if (bitwAnd(0x4000, flags))
            blocks <- c(blocks, bold=int4)
        if (bitwAnd(0x0800, flags))
            blocks <- c(blocks, variations=lengthBlock(int2, int4))
        do.call(mixedBlock, blocks)
    }
}
op252 <- mixedBlock(fontnum=int4,
                    ptsize=int4,
                    fontinfo=markedBlock(fontInfoMarker, xeReadFontInfo))

## XGlyphArray
op253 <- mixedBlock(w=int4,
                    markedBlock(int2,
                                function(marker) {
                                    n <- blockValue(marker)
                                    mixedBlock(xy=vectorBlock(mixedBlock(int4,
                                                                         int4),
                                                              n),
                                               id=vectorBlock(int2, n))
                                },
                                markerLabel="n",
                                blockLabel="glyphs"))

## XStringGlyphArray
op254 <- mixedBlock(markedBlock(int2,
                                function(marker) {
                                    n <- blockValue(marker)
                                    vectorBlock(ASCIIchar, 2*n)
                                },
                                markerLabel="n",
                                blockLabel="text"),
                    w=int4,
                    markedBlock(int2,
                                function(marker) {
                                    n <- blockValue(marker)
                                    mixedBlock(xy=vectorBlock(mixedBlock(int4,
                                                                         int4),
                                                              n),
                                               id=vectorBlock(int2, n))
                                },
                                markerLabel="n",
                                blockLabel="glyphs"))

################################################################################
## Printing

## XFontDef 
print_op_252 <- function(op) {
    ## print(op)
    fontnum <- blockValue(op$blocks$op.opparams.fontnum)
    ptsize <- blockValue(op$blocks$op.opparams.ptsize)
    fontname <-
        paste(blockValue(op$blocks$op.opparams.fontinfo.marker.fontname.block),
              collapse="")
    fontindex <- blockValue(op$blocks$op.opparams.fontinfo.marker.fontindex)
    str <- paste0("x_fnt_def    ",
                  "fontnum=", fontnum, ", ",
                  "ptsize=", ptsize, "\n             ",
                  "fontname=", fontname, " [", fontindex, "]", "\n")
    cat(str)
}

## XGlyphArray
print_op_253 <- function(op) {
    ## print(op)
    n <- blockValue(op$blocks$op.opparams.n)
    xIndex <- 2*(1:n) - 1
    yIndex <- xIndex + 1
    glyphX <- unlist(lapply(xIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    glyphY <- unlist(lapply(yIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    str <- paste0("x_glyph      ",
                  "id=", blockValue(op$blocks$op.opparams.glyphs.id), ", ",
                  "x=", glyphX, ", y=", glyphY, collapse="\n")
    cat(str, "\n")
}

## XStringGlyphArray (XeTeX)
print_op_254 <- function(op) {
    ## print(op)
    n <- blockValue(op$blocks$op.opparams.n)
    xIndex <- 2*(1:n) - 1
    yIndex <- xIndex + 1
    glyphX <- unlist(lapply(xIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    glyphY <- unlist(lapply(yIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    str <- c(paste0("x_glyph_str  '",
                    blockValue(op$blocks$op.opparams.text), "'\n"),
             paste0("x_glyph      ",
                    "id=", blockValue(op$blocks$op.opparams.glyphs.id), ", ",
                    "x=", glyphX, ", y=", glyphY, 
                    collapse="\n"))
    cat(str, "\n")
}

################################################################################
## fonts

xe_identical_font <- function(op1, op2) {
    identical(blockValue(op1$blocks$op.opcode),
              blockValue(op2$blocks$op.opcode)) &&
    identical(blockValue(op1$blocks$op.opparams.fontinfo.marker.fontname.block),
              blockValue(op2$blocks$op.opparams.fontinfo.marker.fontname.block)) &&
    identical(blockValue(op1$blocks$op.opparams.fontinfo.marker.fontindex),
              blockValue(op2$blocks$op.opparams.fontinfo.marker.fontindex)) &&
    identical(blockValue(op1$blocks$op.opparams.ptsize),
              blockValue(op2$blocks$op.opparams.ptsize))
}

font_info_252 <- function(op) {
    fonts <- get("fonts")
    fontnum <- blockValue(op$blocks$op.opparams.fontnum) + 1
    
    if (is.null(fonts[[fontnum]]) ||
        !(xe_identical_font(op, fonts[[fontnum]]$op))) {
        
        ptsize <- blockValue(op$blocks$op.opparams.ptsize)
        fontfile <-
            paste(blockValue(op$blocks$op.opparams.fontinfo.marker.fontname.block),
                  collapse="")
        fontindex <- blockValue(op$blocks$op.opparams.fontinfo.marker.fontindex)
        
        fonts[[fontnum]] <- list(file=fontfile, index=fontindex)
        mag <- get("mag")
        fonts[[fontnum]]$size <- 72*fromTeX(ptsize)/25.4
        fonts[[fontnum]]$op <- op
        fonts[[fontnum]]$TeXfont <- FALSE
        set("fonts", fonts)
    }
}

################################################################################
## metrics

## See comments in ttxCharWidth in luatex.R
xeCharWidth <- function(index, fonts, f, fontsize=10, cex=1) {
    if (fonts[[f]]$TeXfont) {
        stop("Char metrics not available for TeX/Type1 fonts")
    } else {
        device <- get("device")
        font <- getFontFile(fonts[[f]]$file)
        fontfile <- font$file
        filesuffix <- font$suffix
        
        name <- glyphNameFromIndex(index, device, fontfile, filesuffix)
        width <- widthFromGlyphName(name, fontfile, filesuffix)
        headTTX <- getHead(fontfile, filesuffix)
        unitsPerEm <-
            as.numeric(xml_text(xml_find_first(headTTX,
                                               "//unitsPerEm/@value")))
        widthPts <- floor(fontsize + .5)*cex*
            (round(width/(unitsPerEm/1000)))/1000
        xtoTeX(unit(widthPts, "bigpts"))
    }
}

metric_info_252 <- op_ignore

metric_info_253 <- function(op) {
    ## Check position BEFORE text
    updateHoriz(get("h"))
    updateVert(get("v"))
    fonts <- get("fonts")
    f <- get("f")
    ## Update for all text positions

    ## FIXME:  this will only be good for bottom-left
    ##         top-right needs metric info as well
    
    n <- blockValue(op$blocks$op.opparams.n)
    xIndex <- 2*(1:n) - 1
    yIndex <- xIndex + 1
    glyphX <- unlist(lapply(xIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    glyphY <- unlist(lapply(yIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    
    id <- blockValue(op$blocks$op.opparams.glyphs.id)
    widths <- unlist(lapply(id, xeCharWidth, fonts, f))
    ## Move to end of glyphs and check position again
    set("h", get("h") + sum(widths))
    updateHoriz(get("h"))
}

metric_info_254 <- metric_info_253

################################################################################
## glyphs

glyph_op_252 <- op_ignore

glyph_op_253 <- function(op) {
    x <- get("h")
    y <- get("v")
    fonts <- get("fonts")
    f <- get("f")

    n <- blockValue(op$blocks$op.opparams.n)
    xIndex <- 2*(1:n) - 1
    yIndex <- xIndex + 1
    glyphX <- unlist(lapply(xIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    glyphY <- unlist(lapply(yIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    id <- blockValue(op$blocks$op.opparams.glyphs.id)
    ## These need to be cached as part of the font
    weight <- fontWeight(fonts, f)
    style <- fontStyle(fonts, f)
    family <- ttxFontFamily(fonts, f)
    mapply(function(x, y, index, filename, fontindex) {
               addGlyph(glyph(x, y,
                              "", index,
                              family=family, weight=weight, style=style,
                              size=fonts[[f]]$size, filename, fontindex))
           },
           fromTeX(x + glyphX - get("left")),
           fromTeX(y + glyphY - get("top")),
           id, fonts[[f]]$file, fonts[[f]]$index)
    widths <- unlist(lapply(id, xeCharWidth, fonts, f))
    set("h", get("h") + sum(widths))
}

glyph_op_254 <- glyph_op_253

################################################################################
## typeset

typeset_op_252 <- op_ignore

typeset_op_253 <- function(op) {
    x <- get("h")
    y <- get("v")
    fonts <- get("fonts")
    f <- get("f")
    colour <- get("colour")

    n <- blockValue(op$blocks$op.opparams.n)
    xIndex <- 2*(1:n) - 1
    yIndex <- xIndex + 1
    glyphX <- unlist(lapply(xIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    glyphY <- unlist(lapply(yIndex,
                            function(i) {
                                name <- paste0("op.opparams.glyphs.xy", i)
                                blockValue(op$blocks[[name]])
                            }))
    id <- blockValue(op$blocks$op.opparams.glyphs.id)
    ## These need to be cached as part of the font
    weight <- fontWeight(fonts, f)
    style <- fontStyle(fonts, f)
    family <- ttxFontFamily(fonts, f)
    mapply(function(x, y, index, filename, fontindex) {
               addGlyph(glyph(x, y,
                              "", index,
                              family=family, weight=weight, style=style,
                              size=fonts[[f]]$size, filename, fontindex,
                              colour[1]))
           },
           fromTeX(x + glyphX - get("left")),
           fromTeX(y + glyphY - get("top")),
           id, fonts[[f]]$file, fonts[[f]]$index)
    widths <- unlist(lapply(id, xeCharWidth, fonts, f))
    set("h", get("h") + sum(widths))
}

typeset_op_254 <- typeset_op_253

################################################################################
## grobs

op_no_support <- function(op) {
    warning("No support for converting XeTeX XDV to grobs")
}

grid_op_252 <- op_no_support

grid_op_253 <- op_no_support

grid_op_254 <- op_no_support

################################################################################
## engine

xeEngine <- function(engine="xelatex",
                     options="--no-pdf",
                     special=combineSpecials(previewSpecial, colourSpecial)) {
    TeXengine(engine, options, special=special, dviSuffix=".xdv")
}

xelatexEngine <- xeEngine()

xePreamble <- function(font="Latin Modern Roman", preview=TRUE, colour=TRUE) {
    preamble <- c("\\documentclass{standalone}",
                  "\\usepackage{fontspec}",
                  "\\usepackage{unicode-math}",
                  paste0("\\setmainfont{", font, "}"))
    if (preview) 
        preamble <- c(preamble, previewPreamble)
    if (colour)
        preamble <- c(preamble, colourPreamble)
    preamble <- c(preamble, "\\begin{document}")
    if (preview)
        preamble <- c(preamble, previewStart)
    preamble
}

xePostamble <- function(preview=TRUE) {
    postamble <- "\\end{document}"
    if (preview) 
        postamble <- c(previewEnd, postamble)
    postamble
}

xelatexGrob <- function(tex, ...,
                        preamble=xePreamble(),
                        postamble=xePostamble(),
                        engine=xelatexEngine) {
    latexGrob(tex, ..., 
              preamble=preamble, postamble=postamble, engine=engine)
}

grid.xelatex <- function(...) {
    grid.draw(xelatexGrob(...))
}
