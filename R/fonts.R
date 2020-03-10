
################################################################################
## Font info

## ignore most ops
for (i in 0:255) {
    assign(paste0("font_info_", i), op_ignore)
}

## bop
font_info_139 <- op_bop

## font_def<i>
font_info_243 <- op_font_def

## pre
font_info_247 <- op_pre

readFontInfo <- function(op) {
    opcode <- blockValue(op$blocks$op.opcode)
    base::get(paste0("font_info_", opcode))(op)
}

dviFonts <- function(x, device, engine) {
    UseMethod("dviFonts")
}

dviFonts.DVI <- function(x, device, engine) {
    set("device", device)
    set("engine", engine)
    invisible(lapply(x, readFontInfo))
    info <- list(fonts=get("fonts"),
                 device=device)
    class(info) <- "DVIfontInfo"
    info
}

dviFonts.character <- function(x, device, engine) {
    dviFonts(readDVI(x), device, engine)
}

################################################################################

## We can only specify a font family to R's Cairo graphics device
## (and fontconfig is used to select a font from that family).
## We get a font family from the DVI font name by looking up the
## AFM file corresponding to the DVI font name and extracting
## the FamilyName from that AFM file.  We also extract the FullName
## from the AFM file.
## The font family is not specific enough for fontconfig to get
## the exact font that we need (e.g., "Computer Modern" matches
## lots of different TeX fonts), so we generate a temporary
## config file that ensures that the font family we ask for
## will be matched with a font that has a postscriptname property
## equal to the FullName that we extracted from the AFM file.

initFontDir <- function() {
    fontDir <- "LuaTeXFonts"
    tmpFontDir <- file.path(tempdir(), fontDir)
    if (!dir.exists(tmpFontDir)) {
        dir.create(tmpFontDir)
    }
    tmpFontDir
}

initFontConfig <- function() {
    tmpdir <- file.path(tempdir(), "dvir")
    dir.create(tmpdir)
    configFile <- file.path(tmpdir, "10-dvir-fonts.conf")
    ## Ensure that ~/.fonts.conf.d/ exists
    if (!dir.exists("~/.fonts.conf.d/")) {
        dir.create("~/.fonts.conf.d/")
    }
    config <- xml_new_root(xml_dtd("fontconfig", system_id="fonts.dtd"))
    xml_add_child(config, "fontconfig")
    xml_add_child(config, xml_comment("include TeX fonts"))
    xml_add_child(config, "dir",
                  dirname(system("kpsewhich cmr10 --format=.pfb", intern=TRUE)))
    ## Special case cmex10 
    xml_add_child(config, xml_comment("include custom cmexunicode10 font"))
    xml_add_child(config, "dir", system.file("fonts", package="dvir"))
    ## Include 'dvir' custom fonts
    fontDir <- initFontDir()
    xml_add_child(config, xml_comment("include custom 'dvir' fonts"))
    xml_add_child(config, "dir", fontDir)
    ## cat(as.character(fontconfig))
    set("fontconfig", config)
    write_xml(config, configFile)
    set("fontconfigFile", configFile)
    set("fontcache", NULL)
}

addFontConfig <- function(family, psname) {
    fontcache <- get("fontcache")
    if (is.null(fontcache) ||
        !paste(family, psname) %in% fontcache) {
        fontconfig <- xml_root(get("fontconfig"))
        match <- xml_add_child(fontconfig, "match", target="pattern")
        test <- xml_add_child(match, "test", name="family", compare="eq")
        xml_add_child(test, "string", paste(family, psname))
        edit <- xml_add_sibling(test, "edit", name="family",
                                mode="assign", binding="strong")
        xml_add_child(edit, "string", family)
        edit <- xml_add_sibling(edit, "edit", name="postscriptname",
                                mode="assign", binding="strong")
        xml_add_child(edit, "string", psname)
        set("fontconfig", fontconfig)
        configFile <- get("fontconfigFile")
        write_xml(fontconfig, configFile)
        set("fontcache", c(fontcache, paste(family, psname)))
        ## ~/.fonts.conf.d/ is deprecated in favour of
        ## $XDG_CONFIG_HOME/fontconfig/conf.d/ in recent versions of
        ## fontconfig; cover both options.
        file.copy(configFile, "~/.fonts.conf.d", overwrite=TRUE)
        file.copy(configFile, "~/fontconfig/conf.d", overwrite=TRUE)
        ## Force reload of config file
        fontconfig_reinit()
    }
}

initFontMap <- function() {
    mapfile <- system("kpsewhich pdftex.map", intern=TRUE)
    set("fontmap", readLines(mapfile))
}

################################################################################

findTeXFontFile <- function(fontname, checksum, suffix=".afm") {
    ## Map fontname to actual font file
    map <- get("fontmap")
    fontline <- grep(paste0("^", fontname, " "), map)
    ## TODO
    ## This needs to be a lot smarter
    ## (see, e.g., pdftex manual for pdftex.map syntax)
    font <- gsub(".+<|[.]pfb$", "", map[fontline])
    file <- system(paste0("kpsewhich ", font,
                          " --format=", suffix),
                   intern=TRUE)
    if (length(file) == 0) {
        ## Try harder
        file <- system(paste0("kpsewhich ", font,
                              " --format=", suffix, " --must-exist"),
                       intern=TRUE)
    }
    if (length(file) == 0) {
        ## TODO
        ## Font substitution ?
    }
    ## TODO
    ## Check checksum
    file
}

## TODO
## This ASSUMES original TeX font naming
fontSize <- function(fontname) {
    as.numeric(gsub("^[^0-9]+", "", fontname))
}
   
definePostScriptFont <- function(fontname) {
    afmFile <- findTeXFontFile(fontname, suffix=".afm")
    pfbFile <- findTeXFontFile(fontname, suffix=".pfb")
    afm <- readLines(afmFile)
    enc <- fontEnc(afmFile)
    fullname <- paste(strsplit(afm[grep("^FullName", afm)], " ")[[1]][-1],
                      collapse=" ")
    fontMissing <- is.null(postscriptFonts(fontname)[[1]])
    if (fontMissing) {
        fontdef <- Type1Font(fontname,
                             rep(afmFile, 4),
                             encoding=enc[1])
        args <- list(fontdef)
        names(args) <- fontname
        do.call(postscriptFonts, args)
        ## NOTE: because we cannot access char zero 
        ##       (cannot have null char in an R string)
        ##       create a separate encoding file just for char zero
        fontnameZero <- paste0(fontname, "Zero")
        fontdefZero <- Type1Font(fontnameZero,
                                 rep(afmFile, 4),
                                 encoding=enc[2])
        args <- list(fontdefZero)
        names(args) <- fontnameZero
        do.call(postscriptFonts, args)
        ## NOTE: because R brute forces char 45 to /minus
        ##       create a separate encoding file just for char 45
        fontnameHyphen <- paste0(fontname, "Hyphen")
        fontdefHyphen <- Type1Font(fontnameHyphen,
                                 rep(afmFile, 4),
                                 encoding=enc[3])
        args <- list(fontdefHyphen)
        names(args) <- fontnameHyphen
        do.call(postscriptFonts, args)
    }
    list(name=fontname,
         afm=afmFile, pfb=pfbFile,
         postscriptname=fullname,
         size=fontSize(fullname))
}

definePDFFont <- function(fontname) {
    afmFile <- findTeXFontFile(fontname, suffix=".afm")
    pfbFile <- findTeXFontFile(fontname, suffix=".pfb")
    afm <- readLines(afmFile)
    enc <- fontEnc(afmFile)
    fullname <- paste(strsplit(afm[grep("^FullName", afm)], " ")[[1]][-1],
                      collapse=" ")
    fontMissing <- is.null(pdfFonts(fontname)[[1]])
    if (fontMissing) {
        fontdef <- Type1Font(fontname,
                             rep(afmFile, 4),
                             encoding=enc[1])
        args <- list(fontdef)
        names(args) <- fontname
        do.call(pdfFonts, args)
        ## NOTE: because we cannot access char zero 
        ##       (cannot have null char in an R string)
        ##       create a separate encoding file just for char zero
        fontnameZero <- paste0(fontname, "Zero")
        fontdefZero <- Type1Font(fontnameZero,
                                 rep(afmFile, 4),
                                 encoding=enc[2])
        args <- list(fontdefZero)
        names(args) <- fontnameZero
        do.call(pdfFonts, args)
        ## NOTE: because R brute forces char 45 to /minus
        ##       create a separate encoding file just for char 45
        fontnameHyphen <- paste0(fontname, "Hyphen")
        fontdefHyphen <- Type1Font(fontnameHyphen,
                                 rep(afmFile, 4),
                                 encoding=enc[3])
        args <- list(fontdefHyphen)
        names(args) <- fontnameHyphen
        do.call(pdfFonts, args)
    }
    list(name=fontname,
         afm=afmFile, pfb=pfbFile,
         postscriptname=fullname,
         size=fontSize(fullname))
}

defineCairoFont <- function(fontname) {
    ## Special case cmex10 for testing
    if (fontname == "cmex10") {
        afm <- readLines(system.file("fonts", "cmexunicode10.afm",
                                     package="dvir"))
    } else {
        afm <- readLines(findTeXFontFile(fontname, suffix=".afm"))
    }
    familyname <- paste(strsplit(afm[grep("^FamilyName", afm)], " ")[[1]][-1],
                        collapse=" ")
    list(family=familyname)
}

defineFont <- function(fontname, device) {
    if (psDevice(device)) {
        defn <- definePostScriptFont(fontname)
    } else if (pdfDevice(device)) {
        defn <- definePDFFont(fontname)
    } else if (cairoDevice(device)) {
        ## Also define PDF font for font metric calculations (see ./metric.R)
        defn <- definePDFFont(fontname)
        defn <- c(defn, defineCairoFont(fontname))
    } else {
        ## TODO
        ## Other devices 
        stop("Graphics device unsupported")
    }
    defn
}

fontFamily <- function(font, char, device) {
    if (psDevice(device) || pdfDevice(device)) {
        if (!is.null(attr(char, "zeroChar"))) {
            paste0(font$name, "Zero")
        } else if (!is.null(attr(char, "hyphenChar"))) {
            paste0(font$name, "Hyphen")
        } else {
            font$name
        }
    } else if (cairoDevice(device)) {
        ## Allow for special font per char
        if (!is.null(attr(char, "family"))) {
            family <- attr(char, "family")
            psname <- attr(char, "postscriptname")
        } else {
            family <- font$family
            psname <- font$postscriptname
        }
        addFontConfig(family, psname)
        paste(family, psname)
    }
}

