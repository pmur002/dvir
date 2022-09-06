
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

ttxFontFamily <- function(fonts, f) {
    font <- getFontFile(fonts[[f]]$file)
    nameTable <- getNameTable(font$file, font$suffix)
    gsub("^[[:space:]]+|[[:space:]]+$", "",
         xml_text(xml_find_first(nameTable, "//namerecord[@nameID = '1']")))
}
