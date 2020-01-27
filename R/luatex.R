
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
               ## Much work to do here!
               stop("set3 not yet supported"),
               ## Have not yet witnessed set4 op
               stop("set4 not yet supported"))
    } else {
        stop("Graphics device unsupported")
    }
}
