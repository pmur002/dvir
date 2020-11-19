
## Do the default font_info_op for most operations
## (this includes font definitions, bop, and pre)
for (i in 0:255) {
    assign(paste0("lua_font_info_", i),
           get(paste0("font_info_", i)))
}

## fnt_num_<i>
## Need to track which font is currently being used (for set3 op)
for (i in 171:234) {
    assign(paste0("lua_font_info_", i), op_fnt_num)
}

## set3 instructions generate a subset font IF first byte is 0x0F
lua_font_info_130 <- function(op) {
    fonts <- get("fonts")
    f <- get("f")
    device <- get("device")
    if (psDevice(device) || pdfDevice(device)) {
        stop("Sorry, no support for non-CM fonts outside Cairo devices (for now)")
    } else if (cairoDevice(device)) {
        raw <- op$blocks$op.opparams$fileRaw
        if (as.numeric(raw[1]) >= 15) {
            fontname <- fonts[[f]]$postscriptname
            index <- as.numeric(as.hexmode(paste(raw[2:3], collapse="")))
            fontfile <- getFontFile(fontname)
            ## Generate special font for char
            customFont <- subsetFont(fontfile, index)
            ## Get special font into font cache (and FontConfig configuration)
            addFontConfig(customFont$family, customFont$postscriptname,
                          ## Special fonts are in temporary directory that
                          ## has already been added to FontConfig configuration
                          dir=NULL)
        }
    }
}

luaReadFontInfo <- function(op) {
    opcode <- blockValue(op$blocks$op.opcode)
    base::get(paste0("lua_font_info_", opcode))(op)
}

## LuaTeX can generate a different format of fnt_def_op
## (specifically, the 'fontname')
## if the user has made use of the 'fontspec' package
## SO, we need different functions to map the font definition
## to an R font (depending on the R graphics device)

## We just retain the font name and discard the extra font information
## about font options (for now) because R graphics does not make use
## of that extra information (for now)
luaFontName <- function(fontname) {
    ## Allow for ...
    ##   filename="fontname:...;..."       = system font
    ##   filename="file:fontname:...;..."  = TeX font
    ##   filename="[fontname]:...;..."     = TeX font
    ##   filename="[fontfile.ttf]:...;..." = local font
    name <- gsub('^"(file:)?|:.+', "", fontname)
    if (grepl("^[[]", name)) {
        ## Check for a local font
        ## We only support fonts in the current working directory so far
        name <- gsub("^[[]|[]]$", "", name)
        if (grepl("[.]", name)) {
            fontfile <- list.files(pattern=name, ignore.case=TRUE)
        } else {
            fontfile <- list.files(pattern=paste0(name, "[.]"),
                                   ignore.case=TRUE)
        }
        if (length(fontfile)) {
            ## A local (rather than system) font has square brackets
            ## and a suffix
            suffix <- gsub(".+[.]", "", fontfile[1])
            ## Generate appropriate format for luaotfload-tool call
            ## This happens to work for different luaTeX versions
            ## because in one, name has no suffix and adding suffix works,
            ## while in other, name has suffix, but adding superfluous suffix
            ## is necessary (!)
            name <- paste0("file:", name, ".", suffix)
            ## Keep information needed for FontConfig
            attr(name, "dir") <- getwd()
        } else {
            ## Otherwise assume this is a TeX font
            ## (that luaotfload-tool will find)
            ## Remove square brackets and any file suffix
            name <- gsub("[[]|[]]|[.]ttf$|[.]otf$", "", name)
        }
    }
    name
}

luaFontInfo <- function(fontname) {
    ## Failure returns NULL
    result <- NULL
    warnings <- NULL
    fontSearchResult <- system(paste0("luaotfload-tool --find=",
                                      fontname),
                               intern=TRUE)
    if (grepl("found!$", fontSearchResult[1])) {
        fontFile <- gsub('^[^"]+"|"$', "", fontSearchResult[2])
    } else {
        stop(sprintf("Lua font not found on system (%s)", fontname))
    }
    ## Try matching exact font file (should work for TTF system fonts)
    fontdb <- fonttable()
    dbline <- grep(normalizePath(fontFile), fontdb$fontfile, ignore.case=TRUE)
    found <- length(dbline) > 0
    if (found) {
        ## If more than one line matches, use first one
        result <- fontdb[dbline[1], ]
    } else if (!found) {
        ## Try OpenType fonts
        sysfonts <- system_fonts()
        match <- grep(normalizePath(fontFile), sysfonts$path, ignore.case=TRUE)
        if (length(match) > 0) {
            ## Mock up a partial fonttable() line
            ## (just containing the information we need)
            ## We cannot produce a .afm file in this case, but
            ## this should only be used on a Cairo device with non-CM font,
            ## so I think that is OK
            result <- data.frame(afmfile="",
                                 fontfile=sysfonts$path[match],
                                 FullName=sysfonts$name[match],
                                 FamilyName=sysfonts$family[match])
            found <- TRUE
        }
    }
    if (!found) {
        ## Try just matching font FullName (back amongst TTF fonts)
        dbline <- grep(fontname, fontdb$FullName, ignore.case=TRUE)
        if (length(dbline)) {
            ## If more than one line matches, use first one
            result <- fontdb[dbline[1], ]
        } else {
            stop(sprintf("Unable to find font info (%s)", fontname))
        }
    }
    ## Add directory for local font
    result$dir <- attr(fontname, "dir")
    result
}

luaDefinePostScriptFont <- function(fontInfo) {
    afmFile <- system.file("metrics", fontInfo$afmfile,
                           package="extrafontdb")
    fontFile <- fontInfo$fontfile
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
        ## NOTE: because R brute forces char 45 to /minus
        ##       create a separate encoding file just for char 45
        fontnameHyphen <- paste0(familyName, "Hyphen")
        fontdefHyphen <- Type1Font(fontnameHyphen,
                                 rep(afmFile, 4),
                                 encoding=enc[3])
        args <- list(fontdefHyphen)
        names(args) <- fontnameHyphen
        do.call(postscriptFonts, args)
    }
    list(name=familyName,
         afm=afmFile, file=fontFile,
         postscriptname=fullName,
         size=10)
}

luaDefinePDFFont <- function(fontInfo) {
    afmFile <- system.file("metrics", fontInfo$afmfile,
                           package="extrafontdb")
    fontFile <- fontInfo$fontfile
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
        fontnameZero <- paste0(familyName, "Zero")
        fontdefZero <- Type1Font(fontnameZero,
                                 rep(afmFile, 4),
                                 encoding=enc[2])
        args <- list(fontdefZero)
        names(args) <- fontnameZero
        do.call(pdfFonts, args)
        ## NOTE: because R brute forces char 45 to /minus
        ##       create a separate encoding file just for char 45
        fontnameHyphen <- paste0(familyName, "Hyphen")
        fontdefHyphen <- Type1Font(fontnameHyphen,
                                 rep(afmFile, 4),
                                 encoding=enc[3])
        args <- list(fontdefHyphen)
        names(args) <- fontnameHyphen
        do.call(pdfFonts, args)
    }
    list(name=familyName,
         afm=afmFile, file=fontFile,
         postscriptname=fullName,
         size=10)
}

luaDefineCairoFont <- function(fontInfo) {
    familyName <- fontInfo$FamilyName
    list(name=familyName,
         file=fontInfo$fontfile,
         postscriptname=fontInfo$FullName,
         family=familyName,
         dir=fontInfo$dir,
         size=10)
}

luaDefineFont <- function(fontname, device) {
    fontFullName <- luaFontName(fontname)
    fontInfo <- luaFontInfo(fontFullName)
    if (is.null(fontInfo)) {
        ## Assume the font is a TeX font
        defn <- defineFont(fontFullName, device)
        defn$TeXfont <- TRUE
    } else {
        if (psDevice(device)) {
            defn <- luaDefinePostScriptFont(fontInfo)
        } else if (pdfDevice(device)) {
            defn <- luaDefinePDFFont(fontInfo)
        } else if (cairoDevice(device)) {
            defn <- luaDefineCairoFont(fontInfo)
            addFontConfig(defn$family, defn$postscriptname, defn$dir)
        } else {
            ## TODO
            ## Other devices
            stop("Graphics device unsupported")
        }
        defn$TeXfont <- FALSE
    }
    defn
}

luaGetChar <- function(raw, fontname, device) {
    if (psDevice(device) || pdfDevice(device)) {
        stop("Sorry, no support for non-CM fonts outside Cairo devices (for now)")
    } else if (cairoDevice(device)) {
        nbytes <- length(raw)
        switch(nbytes,
               ## Single byte is either set_char_i or set1 op
               if (as.numeric(raw) < 128) {
                   rawToUTF8(raw, fontname)
               } else {
                   ## Single byte is assumed to be UTF16BE 
                   ## (first byte assumed 0) from set1 op
                   iconv(list(c(as.raw(0), raw)),
                         from="UTF16BE", to="UTF-8")
               },
               ## Two bytes is assumed to be UTF16BE from set2 op
               iconv(list(raw), from="UTF16BE", to="UTF-8"),
               ## Three bytes is assumed to be UTF32BE from set3 op
               if (as.numeric(raw[1]) >= 15) {
                   ## UNLESS first byte is 0x0F, then ...
                   ## Three bytes is assumed to be non-UNICODE char from set3 op
                   ## Second two bytes are integer index into non-UNICODE glyphs
                   nonUNICODEchar(as.numeric(as.hexmode(paste(raw[2:3],
                                                              collapse=""))),
                                  fontname)
               } else {
                   iconv(list(c(as.raw(0), raw)), from="UTF32BE", to="UTF-8")
               },
               ## Have not yet witnessed set4 op
               stop("set4 not yet supported"))
    } else {
        stop("Graphics device unsupported")
    }
}

luaCharWidth <- function(raw, fonts, f) {
    font <- fonts[[f]]
    if (font$TeXfont) {
        ## Uses PDF device for char metrics even on Cairo device
        charWidth(raw, fonts, f)
    } else {
        device <- get("device")
        ## Calculate char width from TTX metrics
        ttxCharWidth(raw, fonts[[f]]$file, device,
                     fontsize=fonts[[f]]$size,
                     cex=get("scale"))
    }
}

#########################
## Code for character metrics

AdobeGlyphList <- read.table(system.file("adobe", "glyphlist.txt",
                                         package="dvir"),
                             sep=";", col.names=c("name", "code"),
                             stringsAsFactors=FALSE)


## Convert 4-digit hex code to glyph name
AdobeName <- function(code) {
    AdobeGlyphList$name[AdobeGlyphList$code == code]    
}

## Convert 4-digit hex code to glyph name via font Unicode mapping
cmapName <- function(code, fontfile, suffix) {
    ttxfile <- gsub(paste0("[.]", suffix, "$"), "-cmap.ttx", fontfile)
    if (!file.exists(ttxfile)) {
        system(paste0("ttx -t cmap -o ", ttxfile, " ", fontfile))
    }
    cmap <- getTTX(ttxfile)
    ## Format code for cmap
    code <- tolower(gsub("^0*", "0x", code))
    ## cmap table with platformID="0" is UNICODE mapping
    ## NOTE: this may need relaxing to allow for other table formats
    ##       (e.g., cmap_format_6)
    unicodeMap <- paste0("//cmap_format_4[@platformID = '0']/map[@code = '",
                         code, "']")
    name <- xml_attr(xml_find_first(cmap, unicodeMap), "name")
    if (length(name)) {
        name
    } else {
        NULL
    }   
}

## Glyph name from raw bytes
getGlyphName <- function(raw, device, fontfile, filesuffix) {
    if (psDevice(device) || pdfDevice(device)) {
        stop("Sorry, no support for non-CM fonts outside Cairo devices (for now)")
    } else if (cairoDevice(device)) {
        nbytes <- length(raw)
        code <- switch(nbytes,
                       ## Single byte is either set_char_i or set1 op
                       paste0("00", toupper(as.character(raw))),
                       ## Two bytes is assumed to be UTF16BE from set2 op
                       paste(toupper(as.character(raw)), collapse=""),
                       ## Three bytes is assumed to be UTF32BE from set3 op
                       if (as.numeric(raw[1]) >= 15) {
                           ## UNLESS first byte is 0x0F, then ...
                           ## Three bytes is assumed to be non-UNICODE char
                           ##   from set3 op
                           ## Second two bytes are integer index into
                           ##   non-UNICODE glyphs
                           paste(raw[2:3], collapse="")
                       } else {
                           paste(toupper(as.character(raw)), collapse="")
                       },
                       ## Have not yet witnessed set4 op
                       stop("set4 not yet supported"))
        ## May generate more than one option
        name <- switch(nbytes,
                       c(AdobeName(code),
                         cmapName(code, fontfile, filesuffix)),
                       c(AdobeName(code),
                         paste0("uni", code),
                         cmapName(code, fontfile, filesuffix)),
                       ## Find non-UNICODE glyph name
                       if (as.numeric(raw[1]) >= 15) {
                           getGlyph(getGlyphs(fontfile, filesuffix), 
                                    as.numeric(as.hexmode(code)))$name
                       } else {
                           c(AdobeName(code),
                             paste0("uni", code),
                             paste0("u", gsub("^0", "", code)),
                             cmapName(code, fontfile, filesuffix))
                       },
                       stop("set4 not yet supported"))
    } else {
        stop("Graphics device unsupported")
    }
    name
}

widthFromGlyphName <- function(name, hmtx) {
    metric <- NULL
    ## Try more than one name (if there are multiple options)
    while (!length(metric) && length(name)) {
        metric <- xml_find_first(hmtx, paste0("//mtx[@name = '",
                                              name[1],
                                              "']/@width"))
        name <- name[-1]
    }
    as.numeric(xml_text(metric))
}

## Calculate character width from TTX font metrics
ttxCharWidth <- function(raw, fontpath, device, fontsize=10, cex=1) {
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
    ## Get font metric "scale"
    headTTXfile <- file.path(tmpFontDir, paste0(filestub, "-head.ttx"))
    if (!file.exists(headTTXfile)) {
        system(paste0("ttx -t head -o ", headTTXfile, " ", fontfile))
    }
    headTTX <- getTTX(headTTXfile)
    unitsPerEm <- as.numeric(xml_text(xml_find_first(headTTX,
                                                     "//unitsPerEm/@value")))
    ## Convert char num to glyph name
    glyphName <- getGlyphName(raw, device, fontfile, filesuffix)
    ## Find glyph width
    hmtxTTXfile <- file.path(tmpFontDir, paste0(filestub, "-hmtx.ttx"))
    if (!file.exists(hmtxTTXfile)) {
        system(paste0("ttx -t hmtx -o ", hmtxTTXfile, " ", fontfile))
    }
    hmtxTTX <- getTTX(hmtxTTXfile)
    width <- widthFromGlyphName(glyphName, hmtxTTX)
    ## round() to get whole number metrix (at 1000 scale)
    ## floor() to match what PDF_StrWidthUTF8() does
    widthPts <- floor(fontsize + .5)*cex*(round(width/(unitsPerEm/1000)))/1000
    xtoTeX(unit(widthPts, "bigpts"))
}

#########################
## Code to support set3 op
## (3-byte 0Fxxxx, where xxxx is an integer index to the ith
##  non-UNICODE glyph within the font)
## (e.g., ti ligature, which has no UNICODE code point)

## TODO:
## ONLY tested on a single TrueType font (Lato-Light.ttf) so far

getFont <- function(originalfontfile, charIndex) {
    tmpFontDir <- initFontDir()
    filename <- basename(originalfontfile)
    fontsuffix <- "[.](ttf|otf)$"
    if (!grepl(fontsuffix, filename))
        warning("Unrecognised font suffix")
    filesuffix <- gsub(paste0(".+", fontsuffix), "\\1", filename)
    filestub <- gsub(fontsuffix, "", filename)
    fontfile <- file.path(tmpFontDir, filename)
    if (!file.exists(fontfile)) {
        file.copy(originalfontfile, tmpFontDir)
    }
    subfontfile <- file.path(tmpFontDir,
                             paste0(filestub, "-", charIndex,
                                    "-subset.", filesuffix))
    list(font=filestub,
         file=fontfile,
         suffix=filesuffix,
         subfile=subfontfile)
}

getGlyphs <- function(fontfile, suffix) {
    ttxfile <- gsub(paste0("[.]", suffix, "$"), "-GlyphOrder.ttx", fontfile)
    if (!file.exists(ttxfile)) {
        system(paste0("ttx -t GlyphOrder -o ", ttxfile, " ", fontfile))
    }
    getTTX(ttxfile)
}

## Get glyph number of ith non-UNICODE glyph from TTX
getGlyph <- function(glyphs, index) {
    all <- xml_text(xml_find_all(glyphs, "//GlyphID/@name"))
    notUNICODE <- grep("^glyph", all)
    ## The first '- 1' is for zero-based glyph numbering
    ## At some point, LuaTeX counted differently (by one)
    ## Choosing this to be at version 1.0.0 for now;
    ## May need to refining ...
    version <- get("luaVersion")
    if (compareVersion(version, "1.0.0") < 0) {
        index <- index - 1
    }
    ## The second '- 1' is an off-by-one adjustment
    ## (maybe LuaTeX does not count the .notdef char?)
    glyphNum <- (seq_along(all) - 1)[notUNICODE[index]]
    list(index=glyphNum,
         name=all[glyphNum + 1])
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
    if (!file.exists(ttxfile)) {
        system(paste0("ttx ", fontfile))
    }
    ttxfile
}

## Generate a temporary font name
fontNameGen <- function() {
    index <- 0
    function() {
        index <<- index + 1
        sprintf("dvir_font_%03d", index)
    }
}
dvirFontName <- fontNameGen()

## Get index of the desired glyph name within the subsetted font
getGlyphIndex <- function(glyphs, fontfile, suffix, originalName) {
    ttxfile <- gsub(paste0("[.]", suffix, "$"), "-glyf.ttx", fontfile)
    if (!file.exists(ttxfile)) {
        system(paste0("ttx -t glyf -o ", ttxfile, " ", fontfile))
    }
    glyf <- getTTX(ttxfile)
    components <- xml_find_all(glyf,
                               paste0("//TTGlyph[@name = '", originalName,
                                      "']/component"))
    if (length(components)) {
        ## The subsetted font contains the glyph we want PLUS
        ## glyphs that it is composed from
        ## (plus .notdef glyph)
        glyphNames <- xml_attr(components, "glyphName")
        ## Find the order of the component glyphs (in the original font)
        ## and where the desired glyph comes in that order
        glyphID <- function(name) {
            as.numeric(xml_attr(xml_find_first(glyphs,
                                               paste0("//GlyphID[@name = '",
                                                      name, "']")),
                                "id"))
        }
        originalID <- glyphID(originalName)
        glyphIDs <- sapply(glyphNames, glyphID)
        ## + 1 for .notdef
        sum(glyphIDs < originalID) + 2
    } else {
        ## The subsetted font just contains the glyph we want
        ## (plus .notdef glyph)
        2
    }
}

## Modify TTX file to set <name> and <cmap> elements
editTTX <- function(ttxfile, fontname, glyphIndex) {
    ttx <- read_xml(ttxfile)
    ## Replace <name> element
    name <- xml_find_first(ttx, "/ttFont/name")
    newName <- read_xml("<name/>")
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
    ## Find the name of the glyph
    ## This is not obvious if the glyph is a composite of other glyphs
    ## (this is the glyph that was extracted from the full font)
    glyphs <- xml_find_all(ttx, "//GlyphID")
    glyphName <- xml_attr(glyphs[glyphIndex], "name")
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
    if (!file.exists(fontfile)) {
        system(paste0("ttx -o ",
                      fontfile,
                      " ",
                      ttxfile))
    }
    list(name=fontname,
         file=fontfile)
}

set("luaFontCache", list())

fontID <- function(fontfile, charIndex) {
    paste0(fontfile, "-non-UNICODE-glyph-", charIndex)
}

cacheFont <- function(fontfile, charIndex, subsetName) {
    cache <- get("luaFontCache")
    cache[[fontID(fontfile, charIndex)]] <- subsetName
    set("luaFontCache", cache)
}

fontFromCache <- function(fontfile, charIndex) {
    cache <- get("luaFontCache")
    cache[[fontID(fontfile, charIndex)]]
}    

subsetFont <- function(fontfile, charIndex) {
    cachedFont <- fontFromCache(fontfile, charIndex)
    if (is.null(cachedFont)) {
        fontInfo <- getFont(fontfile, charIndex)
        glyphs <- getGlyphs(fontInfo$file, fontInfo$suffix)
        glyphInfo <- getGlyph(glyphs, charIndex)
        extractGlyph(fontInfo$file, glyphInfo$index, fontInfo$subfile)
        ttxfile <- unwrapFont(fontInfo$subfile, fontInfo$suffix)
        subsetName <- dvirFontName()
        subsetGlyphIndex <- getGlyphIndex(glyphs,
                                          fontInfo$file, fontInfo$suffix,
                                          glyphInfo$name)
        subsetTTX <- editTTX(ttxfile, subsetName, subsetGlyphIndex)
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
    fontfile <- getFontFile(fontname)
    customFont <- subsetFont(fontfile, index)
    char <- "A"
    attr(char, "family") <- customFont$family
    attr(char, "postscriptname") <- customFont$postscriptname
    char
}

#########################
## Luaotf cache (to avoid calling luaotfload-tool over and over again)
initLuaOTFcache <- function() {
    set("luaOTFcache", list())
}

cacheLuaOTF <- function(fontname, fontfile) {
    cache <- get("luaOTFcache")
    cache[[fontname]] <- fontfile
    set("luaOTFcache", cache)
}

getFontFile <- function(fontname) {
    cache <- get("luaOTFcache")
    if (is.null(cache) ||
        is.null(cache[[fontname]])) {
        fontfile <- findFontFile(fontname)
        cacheLuaOTF(fontname, fontfile)        
    } else {
        fontfile <- cache[[fontname]]
    }
    fontfile
}

#########################
## TTX cache (to avoid loading TTX files over and over again)

initTTXcache <- function() {
    set("ttxCache", list())
}

cacheTTX <- function(ttxFile, ttx) {
    cache <- get("ttxCache")
    cache[[ttxFile]] <- ttx
    set("ttxCache", cache)
}

getTTX <- function(ttxFile) {
    cache <- get("ttxCache")
    if (is.null(cache) ||
        is.null(cache[[ttxFile]])) {
        ttx <- read_xml(ttxFile)
        cacheTTX(ttxFile, ttx)        
    } else {
        ttx <- cache[[ttxFile]]
    }
    ttx
}


initLua <- function() {
    versText <- system("luatex --version", intern=TRUE)[1]
    version <- gsub(" .+", "", gsub("^[^0-9]+", "", versText))
    set("luaVersion", version)
    initLuaOTFcache()
    initTTXcache()    
}

###############################
## User interface

luaEngine <- function(engine="lualatex",
                      options="--output-format=dvi",
                      readFonts=luaReadFontInfo,
                      fontDef=luaDefineFont,
                      charEnc=luaGetChar,
                      charMetric=luaCharWidth,
                      special=noSpecial) {
    TeXengine(engine, options, readFonts, fontDef, charEnc, charMetric, special)
}

lualatexEngine <- luaEngine()

luaPreamble <- function(font="Times") {
    c("\\RequirePackage{luatex85} % For more recent versions of LuaTeX",
      "\\documentclass[12pt]{standalone}",
      "\\usepackage{fontspec}",
      paste0("\\setmainfont[Mapping=text-tex]{", font, "}"),
      "\\begin{document}",
      "\\selectfont")
}

lualatexGrob <- function(tex, ...,
                         preamble=luaPreamble(),
                         postamble=getOption("dvir.postamble"),
                         engine=lualatexEngine) {
    latexGrob(tex, ..., 
              preamble=preamble, postamble=postamble, engine=engine)
}

grid.lualatex <- function(...) {
    grid.draw(lualatexGrob(...))
}
