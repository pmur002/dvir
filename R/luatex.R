
## LuaTeX can generate a different format of fnt_def_op
## (specifically, the 'fontname')
## if the user has made use of the 'fontspec' package
## SO, we need different functions to map the font definition
## to an R font (depending on the R graphics device)

## The heuristic for a non-computer-modern font is just that the
## font name is surrounded by double-quotes (and contains more than
## just the font name)
plainFont <- function(fontname) {
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
    if (plainFont(fontname)) {
        defineFont(fontname, device)
    } else {
        fontInfo <- luaFontInfo(fontname)
        if (psDevice(device)) {
            defn <- luaDefinePostScriptFont(fontInfo)
        } else if (pdfDevice(device)) {
            defn <- luaDefinePDFFont(fontInfo)
        } else if (cairoDevice(device)) {
            ## Also define PDF font for font metric calculations
            ## (see ./metric.R)
            defn <- luaDefinePDFFont(fontInfo)
            defn <- c(defn, luaDefineCairoFont(fontInfo))
        } else {
            ## TODO
            ## Other devices
            stop("Graphics device unsupported")
        }
        defn
    }
}

