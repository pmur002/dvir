
## Code for converting between .ttf/.otf and .ttx files

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

getFontFile <- function(fontpath) {
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
    list(file=fontfile, suffix=filesuffix)
}

## Get a single table
getTable <- function(table, fontfile, suffix, replace=table) {
    ttxfile <- gsub(paste0("[.]", suffix, "$"),
                    paste0("-", replace, ".ttx"), fontfile)
    if (!file.exists(ttxfile)) {
        system(paste0("ttx -t ", table, " -o ", ttxfile, " ", fontfile))
    }
    getTTX(ttxfile)
}

getHead <- function(fontfile, suffix) {
    getTable("head", fontfile, suffix)
}

getHHea <- function(fontfile, suffix) {
    getTable("hhea", fontfile, suffix)
}

getGlyphs <- function(fontfile, suffix) {
    getTable("GlyphOrder", fontfile, suffix)
}

getMetrics <- function(fontfile, suffix) {
    getTable("hmtx", fontfile, suffix)
}

getOS2 <- function(fontfile, suffix) {
    getTable("OS/2", fontfile, suffix, "OS2")
}

getHHea <- function(fontfile, suffix) {
    getTable("hhea", fontfile, suffix)
}

getGlyf <- function(fontfile, suffix) {
    getTable("glyf", fontfile, suffix)
}

getNameTable <- function(fontfile, suffix) {
    getTable("name", fontfile, suffix)
}

## Glyph name from glyph index
glyphNameFromIndex <- function(index, device, fontfile, filesuffix) {
    if (psDevice(device) || pdfDevice(device)) {
        stop("Sorry, no support for non-CM fonts outside Cairo devices")
    } else if (cairoDevice(device)) {
        glyphs <- getGlyphs(fontfile, filesuffix)
        xml_text(xml_find_first(glyphs,
                                paste0("//GlyphID[@id = '", index, "']/@name")))
        
    }
}

widthFromGlyphName <- function(name, fontfile, filesuffix) {
    metrics <- getMetrics(fontfile, filesuffix)
    metric <- NULL
    ## Try more than one name (if there are multiple options)
    while (!length(metric) && length(name)) {
        metric <- xml_find_first(metrics, paste0("//mtx[@name = '",
                                                 name[1],
                                                 "']/@width"))
        name <- name[-1]
    }
    as.numeric(xml_text(metric))
}

ascentFromGlyphName <- function(name, fontfile, filesuffix) {
    glyfs <- getGlyf(fontfile, filesuffix)
    ascent <- NULL
    ## Try more than one name (if there are multiple options)
    while (!length(ascent) && length(name)) {
        ascent <- xml_find_first(glyfs, paste0("//TTGlyph[@name = '",
                                               name[1],
                                               "']/@yMax"))
        name <- name[-1]
    }
    if (is.null(ascent)) {
        ## Just use the font ascent
        warning("Unable to find glyph ascent; using font ascent")
        hhea <- getHHea(fontfile, filesuffix)
        ascent <- xml_find_first(hhea, "//ascent/@value")
    }
    as.numeric(xml_text(ascent))
}

descentFromGlyphName <- function(name, fontfile, filesuffix) {
    glyfs <- getGlyf(fontfile, filesuffix)
    descent <- NULL
    ## Try more than one name (if there are multiple options)
    while (!length(descent) && length(name)) {
        descent <- xml_find_first(glyfs, paste0("//TTGlyph[@name = '",
                                                name[1],
                                                "']/@yMin"))
        name <- name[-1]
    }
    if (is.null(descent)) {
        ## Just use the font ascent
        warning("Unable to find glyph descent; using font descent")
        hhea <- getHHea(fontfile, filesuffix)
        descent <- xml_find_first(hhea, "//descent/@value")
    }
    as.numeric(xml_text(descent))
}

ttxFontFamily <- function(fonts, f) {
    font <- getFontFile(fonts[[f]]$file)
    nameTable <- getNameTable(font$file, font$suffix)
    gsub("^[[:space:]]+|[[:space:]]+$", "",
         xml_text(xml_find_first(nameTable, "//namerecord[@nameID = '1']")))
}
