
psDevice <- function(device) {
    device == "postscript"
}

pdfDevice <- function(device) {
    device == "pdf"
}

cairoDevice <- function(device) {
    ## X11cairo, cairo_pdf, cairo_ps
    grepl("cairo", device) ||  
        ## NOTE that png(type="Xlib") has name "PNG" (all caps), etc
        device %in% c("svg", "png", "jpeg", "tiff", "bmp")
}

                           
