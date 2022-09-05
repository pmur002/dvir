
## Support for XeTeX's XDV format

################################################################################
## Reading

## XFontDef 
fontInfoMarker <- mixedBlock(flags=uint2,
                             fontname=lengthBlock(int1, ASCIIchar),
                             fontindex=int4)
readFontInfo <- function(marker) {
    if (!blockValue(marker$flags))
        NULL
    else {
        blocks <- NULL
        flags <- marker$flags
        if (bitwAnd(0x0200, flags))
            blocks <- c(blocks, colour=uint4)
        if (bitwAnd(0x1000, flags))
            blocks <- c(blocks, extend=int4)
        if (bitwAnd(0x2000, flags))
            blocks <- c(blocks, slant=int4)
        if (bitwAnd(0x4000, flags))
            blocks <- c(blocks, bold=int4)
        if (bitwAnd(0x0800, flags))
            blocks <- c(blocks, variations=lengthBlock(int2, uint4))
        do.call(mixedBlock, blocks)
    }
}
op252 <- mixedBlock(fontnum=int4,
                    ptsize=int4,
                    fontinfo=markedBlock(fontInfoMarker, readFontInfo))

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
    fontname <- paste(blockValue(op$blocks$op.opparams.fontinfo.marker.fontname.block),
                      collapse="")
    str <- paste0("x_fnt_def    ",
                  "fontnum=", fontnum, ", ",
                  "ptsize=", ptsize, "\n             ",
                  "fontname=", fontname, "\n")
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
    
