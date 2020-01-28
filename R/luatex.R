
## LuaTeX can generate a different format of fnt_def_op
## (specifically, the 'fontname')
## if the user has made use of the 'fontspec' package
## SO, we need different functions to map the font definition
## to an R font (depending on the R graphics device)

## The heuristic for a non-computer-modern font is just that the
## font name is surrounded by double-quotes (and contains more than
## just the font name)
CMfont <- function(fontname) {
    !grepl('^"', fontname)
}

## We just retain the font name and discard the extra font information
## about font options (for now) because R graphics does not make use
## of that extra information (for now)
luaFontName <- function(fontname) {
    gsub('^"|:.+', "", fontname)
}

luaFontInfo <- function(fontname) {
    fontSearchResult <- system(paste0("luaotfload-tool --find=",
                                      luaFontName(fontname)),
                               intern=TRUE)
    if (grepl("found!$", fontSearchResult[1])) {
        fontFile <- gsub('^[^"]+"|"$', "", fontSearchResult[2])
    } else {
        stop("Lua font not found on system")
    }
    fontdb <- extrafont::fonttable()
    dbline <- grep(fontFile, fontdb$fontfile)
    if (length(dbline) == 0) {
        stop("Lua font not registered with 'extrafont'")
    }
    ## If more than one line matches, use first one
    fontdb[dbline[1], ]
}

luaDefinePostScriptFont <- function(fontInfo) {
    afmFile <- system.file("metrics", fontInfo$afmfile,
                           package="extrafontdb")
    pfbFile <- fontInfo$fontfile
    afm <- readLines(afmFile)
    enc <- fontEnc(afmFile)
    fullName <- fontInfo$FullName
    familyName <- fontInfo$FamilyName
    fontMissing <- is.null(postscriptFonts(familyName)[[1]])
    if (fontMissing) {
        fontdef <- Type1Font(familyName,
                             rep(afmFile, 4),
                             encoding=enc[1])
        args <- list(fontdef)
        names(args) <- familyName
        do.call(postscriptFonts, args)
        ## NOTE: because we cannot access char zero 
        ##       (cannot have null char in an R string)
        ##       create a separate encoding file just for char zero
        fontnameZero <- paste0(familyName, "Zero")
        fontdefZero <- Type1Font(fontnameZero,
                                 rep(afmFile, 4),
                                 encoding=enc[2])
        args <- list(fontdefZero)
        names(args) <- fontnameZero
        do.call(postscriptFonts, args)
    }
    list(name=familyName,
         afm=afmFile, pfb=pfbFile,
         postscriptname=fullname,
         size=10)
}

luaDefinePDFFont <- function(fontInfo) {
    afmFile <- system.file("metrics", fontInfo$afmfile,
                           package="extrafontdb")
    pfbFile <- fontInfo$fontfile
    afm <- readLines(afmFile)
    enc <- fontEnc(afmFile)
    fullName <- fontInfo$FullName
    familyName <- fontInfo$FamilyName
    fontMissing <- is.null(pdfFonts(familyName)[[1]])
    if (fontMissing) {
        fontdef <- Type1Font(familyName,
                             rep(afmFile, 4),
                             encoding=enc[1])
        args <- list(fontdef)
        names(args) <- familyName
        do.call(pdfFonts, args)
        ## NOTE: because we cannot access char zero 
        ##       (cannot have null char in an R string)
        ##       create a separate encoding file just for char zero
        ## NOTE: this single-char-font produces some warnings because
        ##       R checks metric info for character 'M' quite a lot
        ##       (and this single-char-font does not contain 'M')
        fontnameZero <- paste0(familyName, "Zero")
        fontdefZero <- Type1Font(fontnameZero,
                                 rep(afmFile, 4),
                                 encoding=enc[2])
        args <- list(fontdefZero)
        names(args) <- fontnameZero
        do.call(pdfFonts, args)
    }
    list(name=familyName,
         afm=afmFile, pfb=pfbFile,
         postscriptname=fullName,
         size=10)
}

luaDefineCairoFont <- function(fontInfo) {
    list(family=fontInfo$FamilyName)
}

luaDefineFont <- function(fontname, device) {
    if (CMfont(fontname)) {
        defn <- defineFont(fontname, device)
        defn$CMfont <- TRUE
    } else {
        fontInfo <- luaFontInfo(fontname)
        if (psDevice(device)) {
            defn <- luaDefinePostScriptFont(fontInfo)
        } else if (pdfDevice(device)) {
            defn <- luaDefinePDFFont(fontInfo)
        } else if (cairoDevice(device)) {
            ## Also get information like PostScriptName
            defn <- luaDefinePDFFont(fontInfo)
            defn <- c(defn, luaDefineCairoFont(fontInfo))
        } else {
            ## TODO
            ## Other devices
            stop("Graphics device unsupported")
        }
        defn$CMfont <- FALSE
    }
    defn
}

luaCharWidth <- function(raw, fonts, f) {
    font <- fonts[[f]]
    if (font$CMfont) {
        ## Uses PDF device for char metrics even on Cairo device
        charWidth(raw, fonts, f)
    } else {
        ## Just use current device for char metrics
        device <- get("device")
        ## FIXME
        if (!cairoDevice(device)) {
            stop("Sorry, no support for non-CM fonts outside Cairo devices (for now)")
        }
        char <- luaGetChar(raw,
                           fonts[[f]]$postscriptname,
                           device)
        family <- fontFamily(fonts[[f]], char, device)
        ## Location (x, y) of text does not matter
        tg <- textGrob(char, 0, 0, 
                       gp=gpar(fontfamily=family,
                               fontsize=fonts[[f]]$size,
                               cex=get("scale")))
        xtoTeX(grobWidth(tg))
    }
}

luaGetChar <- function(raw, fontname, device) {
    if (psDevice(device) || pdfDevice(device)) {
        stop("Sorry, no support for non-CM fonts outside Cairo devices (for now)")
    } else if (cairoDevice(device)) {
        nbytes <- length(raw)
        switch(nbytes,
               ## Single byte is either set_char_i or set1 op
               rawToUTF8(raw, fontname),
               ## Two bytes is assumed to be UTF16BE from set2 op
               iconv(rawToChar(raw), from="UTF16BE", to="UTF-8"),
               ## Three bytes is assumed to be non-UNICODE char from set3 op
               ## First byte is 0x0F
               ## Second two bytes are integer index into non-UNICODE glyphs
               nonUNICODEchar(as.numeric(as.hexmode(paste(raw[2:3], collapse=""))),
                              fontname),
               ## Have not yet witnessed set4 op
               stop("set4 not yet supported"))
    } else {
        stop("Graphics device unsupported")
    }
}

#########################
## Code to support set3 op
## (3-byte 0Fxxxx, where xxxx is an integer index to the ith
##  non-UNICODE glyph within the font)
## (e.g., ti ligature, which has no UNICODE code point)

## TODO:
## ONLY tested on a single TrueType font (Lato-Light.ttf) so far

getFont <- function(fontfile) {
    tmpFontDir <- initFontDir()
    filename <- basename(fontfile)
    fontsuffix <- "[.](ttf|otf)$"
    if (!grepl(fontsuffix, filename))
        warning("Unrecognised font suffix")
    filesuffix <- gsub(paste0(".+", fontsuffix), "\\1", filename)
    filestub <- gsub(fontsuffix, "", filename)
    file.copy(fontfile, tmpFontDir)
    fontfile <- file.path(tmpFontDir, filename)
    subfontfile <- file.path(tmpFontDir,
                             paste0(filestub, "-subset.", filesuffix))
    list(font=filestub,
         file=fontfile,
         suffix=filesuffix,
         subfile=subfontfile)
}

## Get glyph number of ith non-UNICODE glyph from TTX
getGlyph <- function(fontfile, suffix, index) {
    ttxfile <- gsub(paste0("[.]", suffix, "$"), "-GlyphOrder.ttx", fontfile)
    if (file.exists(ttxfile))
        unlink(ttxfile)
    system(paste0("ttx -t GlyphOrder -o ",
                  ttxfile,
                  " ",
                  fontfile))
    glyphs <- read_xml(ttxfile)
    all <- xml_text(xml_find_all(glyphs, "//GlyphID/@name"))
    notUNICODE <- grep("^glyph", all)
    ## The first '- 1' is for zero-based glyph numbering
    ## The second '- 1' is an off-by-one adjustment
    ## (maybe LuaTeX does not count the .notdef char?)
    glyphNum <- (seq_along(all) - 1)[notUNICODE[index - 1]]
    list(index=glyphNum,
         name=all[glyphNum])
}

## Use 'pyftsubset' to subset a single glyph from a font (into a new font)
extractGlyph <- function(fontfile, glyphNum, subfontfile) {
    system(paste0("pyftsubset ",
                  fontfile,
                  " --gids=",
                  glyphNum,
                  " --output-file=",
                  subfontfile,
                  ## Retain name tables
                  " --name-IDs='*'"))
}

## Use 'ttx' to convert .ttf to .ttx
unwrapFont <- function(fontfile, suffix) {
    ttxfile <- gsub(paste0(suffix, "$"), "ttx", fontfile)
    if (file.exists(ttxfile))
        unlink(ttxfile)
    system(paste0("ttx ", fontfile))
    ttxfile
}

## Generate a temporary font name
fontNameGen <- function() {
    index <- 0
    function() {
        index <<- index + 1
        paste0("dvir-font-", index)
    }
}
dvirFontName <- fontNameGen()

## Modify TTX file to set <name> and <cmap> elements
editTTX <- function(ttxfile, fontname) {
    ttx <- read_xml(ttxfile)
    ## Replace <name> element
    name <- xml_find_first(ttx, "/ttFont/name")
    newName <- read_xml("<name/>")
    nameElements <- xml_children(name)
    familyname <- xml_find_first(name, "namerecord[@nameID = '1']")
    xml_set_text(familyname, fontname)
    subfamilyname <- xml_find_first(name, "namerecord[@nameID = '2']")
    fullname <- xml_find_first(name, "namerecord[@nameID = '4']")
    xml_set_text(fullname, paste(fontname, xml_text(subfamilyname)))
    psname <- xml_find_first(name, "namerecord[@nameID = '6']")
    xml_set_text(psname, fontname)
    xml_add_child(newName, familyname)
    xml_add_child(newName, subfamilyname)
    xml_add_child(newName, fullname)
    xml_add_child(newName, psname)
    xml_replace(name, newName)
    ## Find the name of the second-to-last glyph
    ## (this is the glyph that was extracted from the full font)
    glyphs <- xml_find_all(ttx, "//GlyphID")
    glyphName <- xml_attr(glyphs[length(glyphs) - 1], "name")
    ## Add to <cmap> element
    cmap <- xml_find_first(ttx, "/ttFont/cmap")
    cmapTable <-
        read_xml('<cmap_format_4 platformID="0" platEncID="3" language="0"/>')
    xml_add_child(cmapTable, "map", code="0x41", name=glyphName)
    xml_add_child(cmap, cmapTable)
    editedttxfile <- gsub("[.]ttx$", paste0("-edited.ttx"), ttxfile)
    write_xml(ttx, editedttxfile)
    editedttxfile
}

rewrapFont <- function(ttxfile, fontname, suffix) {
    tmpFontDir <- initFontDir()
    fontfile <- file.path(tmpFontDir, paste0(fontname, ".", suffix))
    if (file.exists(fontfile))
        unlink(fontfile)
    system(paste0("ttx -o ",
                  fontfile,
                  " ",
                  ttxfile))
    list(name=fontname,
         file=fontfile)
}

set("fontCache", list())

cacheFont <- function(fontfile, charIndex, subsetName) {
    cache <- get("fontCache")
    cache[[paste0(fontfile, charIndex)]] <- subsetName
    set("fontCache", cache)
}

fontFromCache <- function(fontfile, charIndex) {
    cache <- get("fontCache")
    cache[[paste0(fontfile, charIndex)]]
}    

subsetFont <- function(fontfile, charIndex) {
    cachedFont <- fontFromCache(fontfile, charIndex)
    if (is.null(cachedFont)) {
        fontInfo <- getFont(fontfile)
        glyphInfo <- getGlyph(fontInfo$file, fontInfo$suffix, charIndex)
        extractGlyph(fontInfo$file, glyphInfo$index, fontInfo$subfile)
        ttxfile <- unwrapFont(fontInfo$subfile, fontInfo$suffix)
        subsetName <- dvirFontName()
        subsetTTX <- editTTX(ttxfile, subsetName)
        rewrapFont(subsetTTX, subsetName, fontInfo$suffix)
        cacheFont(fontfile, charIndex, subsetName)
    } else {
        subsetName <- cachedFont
    }
    list(family=subsetName,
         postscriptname=subsetName)
}

findFontFile <- function(fontname) {
    result <- system(paste0("luaotfload-tool --find='", fontname, "'"),
                     intern=TRUE)
    gsub('[^"]+ "|"$', "", result[2])
}

nonUNICODEchar <- function(index, fontname) {
    fontfile <- findFontFile(fontname)
    customFont <- subsetFont(fontfile, index)
    char <- "A"
    attr(char, "family") <- customFont$family
    attr(char, "postscriptname") <- customFont$postscriptname
    char
}
