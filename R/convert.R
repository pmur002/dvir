
## Conversions
## to mm
fromTeX <- function(x) {
    factor <- get("scale")*get("num")/get("den")*1000/get("mag")/10^4
    x*factor
}
minThickness <- function(x, device) {
    ## For raster devices, do not allow thickness to be below lwd=1 (in mm)
    if (rasterDevice(device)) {
        y <- fromTeX(x)
        max(y, 25.4/72)
    } else {
        x
    }
}
## from 'grid' units
xtoTeX <- function(x) {
    factor <- get("scale")*get("num")/get("den")*1000/get("mag")/10^4
    convertX(x, "mm", valueOnly=TRUE)/factor
}
ytoTeX <- function(x) {
    factor <- get("scale")*get("num")/get("den")*1000/get("mag")/10^4
    convertY(x, "mm", valueOnly=TRUE)/factor
}

