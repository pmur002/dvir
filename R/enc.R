
################################################################################
## Tex byte to UTF8

## Provide UTF8 characters in each slot of an encoding file

## OT1
## https://en.wikipedia.org/wiki/OT1_encoding
## generateOT1 <- function() {
##     library(rvest)
##     html <- read_html("https://en.wikipedia.org/wiki/OT1_encoding",
##                       encoding="UTF-8")
##     table1 <- html_nodes(html, "table")[1]
##     cells <- html_nodes(table1, "td")[c(-1, -130)]
##     unicode <- paste0("\\U", substr(html_text(cells), 2, 5))
##     dump("unicode", "")
## }

OT1 <- c("\U0393", "\U0394", "\U0398", "\U039B", "\U039E", "\U03A0", 
         "\U03A3", "\U03A5", "\U03A6", "\U03A8", "\U03A9", "\UFB00", 
         "\UFB01", "\UFB02", "\UFB03", "\UFB04", "\U0131", "\U0237", 
         "\U0060", "\U00B4", "\U02C7", "\U02D8", "\U02C9", "\U02DA", 
         "\U00B8", "\U00DF", "\U00E6", "\U0153", "\U00F8", "\U00C6", 
         "\U0152", "\U00D8", "\U0337", "\U0021", "\U201D", "\U0023", 
         "\U0024", "\U0025", "\U0026", "\U2019", "\U0028", "\U0029", 
         "\U002A", "\U002B", "\U002C", "\U002D", "\U002E", "\U002F", 
         "\U0030", "\U0031", "\U0032", "\U0033", "\U0034", "\U0035", 
         "\U0036", "\U0037", "\U0038", "\U0039", "\U003A", "\U003B", 
         "\U00A1", "\U003D", "\U00BF", "\U003F", "\U0040", "\U0041", 
         "\U0042", "\U0043", "\U0044", "\U0045", "\U0046", "\U0047", 
         "\U0048", "\U0049", "\U004A", "\U004B", "\U004C", "\U004D", 
         "\U004E", "\U004F", "\U0050", "\U0051", "\U0052", "\U0053", 
         "\U0054", "\U0055", "\U0056", "\U0057", "\U0058", "\U0059", 
         "\U005A", "\U005B", "\U201C", "\U005D", "\U02C6", "\U02D9", 
         "\U2018", "\U0061", "\U0062", "\U0063", "\U0064", "\U0065", 
         "\U0066", "\U0067", "\U0068", "\U0069", "\U006A", "\U006B", 
         "\U006C", "\U006D", "\U006E", "\U006F", "\U0070", "\U0071", 
         "\U0072", "\U0073", "\U0074", "\U0075", "\U0076", "\U0077", 
         "\U0078", "\U0079", "\U007A", "\U2013", "\U2014", "\U02DD", 
         "\U02DC", "\U00A8")

CMR <- OT1

## OML
## https://en.wikipedia.org/wiki/OML_encoding
## generateOML <- function() {
##     library(rvest)
##     html <- read_html("https://en.wikipedia.org/wiki/OML_encoding",
##                       encoding="UTF-8")
##     table1 <- html_nodes(html, "table")[1]
##     cells <- html_nodes(table1, "td")[c(-1, -130)]
##     unicode <- paste0("\\U", substr(html_text(cells), 2, 5))
##     dump("unicode", "")
## }

OML <- c("\U0393", "\U0394", "\U0398", "\U039B", "\U039E", "\U03A0", 
         "\U03A3", "\U03A5", "\U03A6", "\U03A8", "\U03A9", "\U03B1", 
         "\U03B2", "\U03B3", "\U03B4", "\U03B5", "\U03B6", "\U03B7", 
         "\U03B8", "\U03B9", "\U03BA", "\U03BB", "\U03BC", "\U03BD", 
         "\U03BE", "\U03C0", "\U03C1", "\U03C3", "\U03C4", "\U03C5", 
         "\U03C6", "\U03C7", "\U03C8", "\U03C9", "\U220A", "\U03D1", 
         "\U03D6", "\U03F1", "\U03C2", "\U03D5", "\U21BC", "\U21BD", 
         "\U21C0", "\U21C1", "\UF8FC", "\UF8FD", "\U25B9", "\U25C3", 
         "\U0030", "\U0031", "\U0032", "\U0033", "\U0034", "\U0035", 
         "\U0036", "\U0037", "\U0038", "\U0039", "\U002E", "\U002C", 
         "\U003C", "\U002F", "\U003E", "\U22C6", "\U2202", "\U0041", 
         "\U0042", "\U0043", "\U0044", "\U0045", "\U0046", "\U0047", 
         "\U0048", "\U0049", "\U004A", "\U004B", "\U004C", "\U004D", 
         "\U004E", "\U004F", "\U0050", "\U0051", "\U0052", "\U0053", 
         "\U0054", "\U0055", "\U0056", "\U0057", "\U0058", "\U0059", 
         "\U005A", "\U266D", "\U266E", "\U266F", "\U25E1", "\U25E0", 
         "\U2213", "\U0061", "\U0062", "\U0063", "\U0064", "\U0065", 
         "\U0066", "\U0067", "\U0068", "\U0069", "\U006A", "\U006B", 
         "\U006C", "\U006D", "\U006E", "\U006F", "\U0070", "\U0071", 
         "\U0072", "\U0073", "\U0074", "\U0075", "\U0076", "\U0077", 
         "\U0078", "\U0079", "\U007A", "\U0131", "\U0237", "\U2118", 
         "\U20D7", "\U0311")

CMM <- OML

## OMS
## https://en.wikipedia.org/wiki/OMS_encoding
## Manual modifications:
## TeX 106 "bar" -> UNICODE 007C "vertical line" (instead of 2223 "divides")
## generateOMS <- function() {
##     library(rvest)
##     html <- read_html("https://en.wikipedia.org/wiki/OMS_encoding",
##                       encoding="UTF-8")
##     table1 <- html_nodes(html, "table")[1]
##     cells <- html_nodes(table1, "td")[c(-1, -130)]
##     unicode <- paste0("\\U", substr(html_text(cells), 2, 5))
##     dump("unicode", "")
## }

OMS <- c("\U2212", "\U22C5", "\U00D7", "\U2217", "\U00F7", "\U22C4", 
         "\U00B1", "\U2213", "\U2295", "\U2296", "\U2297", "\U2298", 
         "\U2299", "\U25CB", "\U2218", "\U2219", "\U223D", "\U2261", 
         "\U2286", "\U2287", "\U2264", "\U2265", "\U227C", "\U227D", 
         "\U223C", "\U2248", "\U2282", "\U2283", "\U226A", "\U226B", 
         "\U227A", "\U227B", "\U2190", "\U2192", "\U2191", "\U2193", 
         "\U2194", "\U2197", "\U2198", "\U2243", "\U21D0", "\U21D2", 
         "\U21D1", "\U21D3", "\U21D4", "\U2196", "\U2199", "\U221E", 
         "\U2032", "\U221D", "\U2208", "\U220B", "\U25B3", "\U25BD", 
         "\U030D", "\U0338", "\U2200", "\U2203", "\U00AC", "\U2205", 
         "\U2111", "\U211C", "\U22A4", "\U22A5", "\U2135", "\U0041", 
         "\U0042", "\U0043", "\U0044", "\U0045", "\U0046", "\U0047", 
         "\U0048", "\U0049", "\U004A", "\U004B", "\U004C", "\U004D", 
         "\U004E", "\U004F", "\U0050", "\U0051", "\U0052", "\U0053", 
         "\U0054", "\U0055", "\U0056", "\U0057", "\U0058", "\U0059", 
         "\U005A", "\U222A", "\U2229", "\U228E", "\U2227", "\U2228", 
         "\U22A2", "\U22A3", "\U230A", "\U230B", "\U2308", "\U2309", 
         "\U007B", "\U007D", "\U2329", "\U232A", "\U007C", "\U2225", 
         "\U2195", "\U21D5", "\U2216", "\U2240", "\U221A", "\U2210", 
         "\U2207", "\U222B", "\U2294", "\U2293", "\U2291", "\U2292", 
         "\U00A7", "\U2020", "\U2021", "\U00B6", "\U2663", "\U2662", 
         "\U2661", "\U2660")

CMSY <- OMS

## CMX
## /usr/share/texlive/texmf-dist/tex/generic/pdftex/glyphtounicode.tex
## https://ctan.org/tex-archive/macros/latex/contrib/pdfx/glyphtounicode-cmr.tex
## kpsewhich cmex10.afm
## /usr/share/texlive/texmf-dist/fonts/afm/public/amsfonts/cm/cmex10.afm

## generateCMX <- function() {
##     afm <- readLines("encdata/cmex10.afm")
##     chars <- grep("^C ", afm)
##     names <- lapply(strsplit(afm[chars], " "),
##                     function(x) x[8])
##     cmx <- readLines("encdata/glyphtounicode-cmr.tex")
##     default <- readLines("encdata/glyphtounicode.tex")
##     codepoints <- sapply(names,
##                          function(x) {
##                              pattern <- paste0("pdfglyphtounicode[{]", x, "[}]")
##                              line <- grep(pattern, cmx)
##                              if (length(line)) {
##                                  substr(gsub("^.+[{]", "", cmx[line]), 1, 4)
##                              } else {
##                                  line <- grep(pattern, default)
##                                  if (length(line)) {
##                                      substr(gsub("^.+[{]", "", default[line]),
##                                             1, 4)
##                                  } else {
##                                      NA
##                                  }
##                              }
##                          })
##     unicode <- paste0("\\U", codepoints)
##     dump("unicode", "")
## }

CMX <- c("\U0028", "\U0029", "\U005B", "\U005D", "\U230A", "\U230B", 
         "\U2308", "\U2309", "\U007B", "\U007D", "\U27E8", "\U27E9", 
         "\U23D0", "\UED79", "\U002F", "\U005C", "\U0028", "\U0029", 
         "\U0028", "\U0029", "\U005B", "\U005D", "\U230A", "\U230B", 
         "\U2308", "\U2309", "\U007B", "\U007D", "\U27E8", "\U27E9", 
         "\U002F", "\U005C", "\U0028", "\U0029", "\U005B", "\U005D", 
         "\U230A", "\U230B", "\U2308", "\U2309", "\U007B", "\U007D", 
         "\U27E8", "\U27E9", "\U002F", "\U005C", "\U002F", "\U005C", 
         "\U239B", "\U239E", "\U23A1", "\U23A4", "\U23A3", "\U23A6", 
         "\U23A2", "\U23A5", "\U23A7", "\U23AB", "\U23A9", "\U23AD", 
         "\U23A8", "\U23AC", "\U23AA", "\U23D0", "\U239D", "\U23A0", 
         "\U239C", "\U239F", "\U27E8", "\U27E9", "\U2A06", "\U2A06", 
         "\U222E", "\U222E", "\U2A00", "\U2A00", "\U2A01", "\U2A01", 
         "\U2A02", "\U2A02", "\U2211", "\U220F", "\U222B", "\U22C3", 
         "\U22C2", "\U2A04", "\U22C0", "\U22C1", "\U2211", "\U220F", 
         "\U222B", "\U22C3", "\U22C2", "\U2A04", "\U22C0", "\U22C1", 
         "\U2210", "\U2210", "\U02C6", "\U02C6", "\U02C6", "\U02DC", 
         "\U02DC", "\U02DC", "\U005B", "\U005D", "\U230A", "\U230B", 
         "\U2308", "\U2309", "\U007B", "\U007D", "\U221A", "\U221A", 
         "\U221A", "\U221A", "\U221A", "\UED6B", "\UED6A", "\UED12", 
         "\U2191", "\U2193", "\UED17", "\UED18", "\UED19", "\UED1A", 
         "\U21D1", "\U21D3", "\U0028", "\U221A", "\U0020", "\U0028", 
         "\U0029", "\U005B", "\U005D", "\U230A", "\U230B", "\U2308", 
         "\U2309", "\U007B", "\U007D", "\U27E8", "\U27E9", "\U23D0", 
         "\UED79", "\U002F", "\U005C", "\U0028", "\U0029", "\U0028", 
         "\U0029", "\U005B", "\U005D", "\U230A", "\U230B", "\U2308", 
         "\U2309", "\U007B", "\U007D", "\U27E8", "\U27E9", "\U002F", 
         "\U005C", "\U0028", "\U21D3", "\U0393", "\U2206", "\U0398", 
         "\U039B", "\U039E", "\U03A0", "\U03A3", "\U03A5", "\U03A6", 
         "\U03A8", "\U2126")

## Using re-encoded CMEX10UNICODE (see ../inst/fonts/CMEX)
## CMEX <- CMX
CMEX <- CMR

## TODO
## This works for Cairo devices ...
## ... BUT may need tweaking for R postscript/pdf devices
rawToUTF8 <- function(x, fontname) {
    ## If necessary, convert from TeX encoding to UTF8
    if (grepl("^CMR", fontname)) {
        CMR[as.integer(x) + 1]
    } else if (grepl("^CMM", fontname)) {
        CMM[as.integer(x) + 1]
    } else if (grepl("^CMEX", fontname)) {
        CMEX[as.integer(x) + 1]
    } else if (grepl("^CMSY", fontname)) {
        CMSY[as.integer(x) + 1]
    } else {
        rawToChar(x)
    }
}

################################################################################
## TeX byte to char

## Generate encoding files from AFM files (order of character names)

fontEnc <- function(afmFile) {
    afm <- readLines(afmFile)
    chars <- grep("^C ", afm)
    names <- lapply(strsplit(afm[chars], " "),
                    function(x) x[8])
    pad <- 256 - length(names)
    if (pad > 0) {
        names <- c(names, rep(".notdef", pad))
    }
    filebase <- basename(gsub("[.]afm", "", afmFile))
    encFile <- file.path(tempdir(), paste0(filebase, ".enc"))
    writeLines(c(paste0("/", basename(filebase), "Encoding ["),
                 paste0("/", names),
                 "]"),
               encFile)
    ## NOTE: because we cannot access char zero 
    ##       (cannot have null char in an R string)
    ##       create a separate encoding file just for char zero
    encFileZero <- file.path(tempdir(), paste0(filebase, "Zero.enc"))
    writeLines(c(paste0("/", basename(filebase), "ZeroEncoding ["),
                 paste0("/", c(".notdef", names[1], rep(".notdef", 254))),
                 "]"),
               encFileZero)
    c(encFile, encFileZero)
}

################################################################################

## TODO
## Devices other than postscript, pdf, or Cairo-based
getChar <- function(raw, fontname, device) {
    if (psDevice(device) || pdfDevice(device)) {
        if (raw == 0) {
            char <- rawToChar(as.raw(1))
            attr(char, "zeroChar") <- TRUE
            char
        } else {
            rawToChar(raw)
        }
    } else if (cairoDevice(device)) {
        rawToUTF8(raw, fontname)
    }
}
