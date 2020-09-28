
## Handlers for \special operators (xxx<i>)

## These handlers contain a metrics handler and a grob handler
ignoreSpecial <- function(op) {}

specialHandler <- function(init=function() {},
                           metric=ignoreSpecial,
                           grid=ignoreSpecial) {
    structure(list(init=init, metric=metric, grid=grid),
              class="DVIRspecial")
}

################################################################################
## Totally ignore special 
noSpecial <- specialHandler()

################################################################################
## Handle special output that is aimed at 'dvir' (from TikZ)

specialInit <- function() {
    set("inPicture", FALSE)
}

## Update the location and bounding box for the figure

## NOTE that all moves have to update h/v
## BUT only drawing should update bbox

## NOTE also that during the metric run we only have DVI locations in "mm";
## we have not yet set up the viewport with correct "native" coordinates

## NOTE also that the TikZ locations are in "pt"s

metricMoveTo <- function(x) {
    xy <- strsplit(x, ",")[[1]]
    picX <- get("pictureX")
    picY <- get("pictureY")
    set("h", xtoTeX(unit(picX, "mm") + unit(as.numeric(xy[1]), "pt")))
    set("v", ytoTeX(unit(picY, "mm") - unit(as.numeric(xy[2]), "pt")))
}

metricLineTo <- function(x) {
    xy <- strsplit(x, ",")[[1]]
    picX <- get("pictureX")
    picY <- get("pictureY")
    updateHoriz(get("h"))
    updateVert(get("v"))
    set("h", xtoTeX(unit(picX, "mm") + unit(as.numeric(xy[1]), "pt")))
    set("v", ytoTeX(unit(picY, "mm") - unit(as.numeric(xy[2]), "pt")))
    updateHoriz(get("h"))
    updateVert(get("v"))
}

metricPathElement <- function(x) {
    tokens <- strsplit(x, " ")[[1]]
    switch(tokens[1],
           moveto=metricMoveTo(tokens[-1]),
           lineto=metricLineTo(tokens[-1]),
           curveto=stop("not yet supported"),
           stop("unsupported path element"))
}

metricSpecial <- function(x) {
    ## Split by ": " (for paths)
    tokens <- strsplit(gsub(" *$", "", x), ":")[[1]]
    if (length(tokens) == 1) {
        ## Nothing to do
    } else {
        ## Path
        lapply(tokens, metricPathElement)
        invisible()
    }
}

specialMetric <- function(op) {
    specialString <- gsub("dvir:: ", "",
                          paste(blockValue(op$blocks$op.opparams.string),
                                collapse=""))
    if (grepl("^begin-picture", specialString)) {
        x <- fromTeX(get("h"))
        y <- fromTeX(get("v"))
        set("pictureX", x)
        set("pictureY", y)
        set("inPicture", TRUE)
    } else if (grepl("^end-picture", specialString)) {
        set("inPicture", FALSE)
    } else {
        if (get("inPicture")) {
            metricSpecial(specialString)
        }
    }
}

## Generate a grob (gTree) for the figure
vpNameGen <- function() {
    vpIndex <- 0
    function() {
        vpIndex <<- vpIndex + 1
        paste0("tikz", vpIndex)
    }
}
vpName <- vpNameGen()

parseMoveTo <- function(x) {
    xy <- strsplit(x, ",")[[1]]
    set("pathX", as.numeric(xy[1]))
    set("pathY", as.numeric(xy[2]))
}
parseLineTo <- function(x) {
    xy <- strsplit(x, ",")[[1]]
    set("pathX", c(get("pathX"), as.numeric(xy[1])))
    set("pathY", c(get("pathY"), as.numeric(xy[2])))
}

drawPathElement <- function(x) {
    tokens <- strsplit(x, " ")[[1]]
    switch(tokens[1],
           moveto=parseMoveTo(tokens[-1]),
           lineto=parseLineTo(tokens[-1]),
           curveto=stop("not yet supported"),
           stop("unsupported path element"))
}

drawNewPath <- function() {
    set("pathX", NULL)
    set("pathY", NULL)
}

drawStroke <- function() {
    picX <- get("pictureX")
    picY <- get("pictureY")
    pathX <- get("pathX")
    pathY <- get("pathY")
    grid.polyline(x=unit(picX, "native") + unit(pathX, "pt"),
                  y=unit(picY, "native") + unit(pathY, "pt"))    
}

parseValueWithUnit <- function(x) {
    unit <- gsub("[0-9.]+", "", x)
    value <- as.numeric(gsub("([0-9.]+).+", "\\1", x))
    switch(unit,
           bp=value/72,
           pt=value/72.27,
           stop("unsupported unit"))
}

parseLineDash <- function(x) {
    if (x == "none") {
        "solid"
    } else {
        ## Convert line-dash to #1234 format
        stop("not yet supported")
    }
}

parseSetting <- function(x) {
    name <- x[1]
    value <- x[2]
    switch(name,
           stroke=eval(str2lang(value)),
           fill=eval(str2lang(value)),
           lwd=96*parseValueWithUnit(value),
           lty=parseLineDash(value),
           stop("unsupported setting"))
}

drawBeginScope <- function(x) {
    if (length(x) == 0) {
        gp <- gpar()
    } else {
        tokens <- strsplit(x, "=")
        names <- sapply(tokens, "[", 1)
        values <- lapply(tokens, parseSetting)
        names(values) <- names
        gp <- do.call(gpar, values)
    }
    topvp <- get("viewport")
    vp <- viewport(gp=gp,
                   xscale=topvp$xscale, yscale=topvp$yscale,
                   name=vpName())
    pushViewport(vp)
}

drawEndScope <- function() {
    popViewport()
}

drawSpecial <- function(x) {
    ## Split by ": " (for paths)
    tokens <- strsplit(gsub(" *$", "", x), ":")[[1]]
    if (length(tokens) == 1) {
        tokens <- strsplit(gsub(" *$", "", tokens), " ")[[1]]
        switch(tokens[1],
               `begin-scope`=drawBeginScope(tokens[-1]),
               `end-scope`=drawEndScope(),
               `new-path`=drawNewPath(),
               `stroke`=drawStroke(),
               stop("Unsupported TikZ special"))
    } else {
        ## Path
        lapply(tokens, drawPathElement)
        invisible()
    }
}

gridSpecial <- function(op) {
    specialString <- gsub("dvir:: ", "",
                          paste(blockValue(op$blocks$op.opparams.string),
                                collapse=""))
    if (grepl("^begin-picture", specialString)) {
        x <- fromTeX(get("h"))
        y <- fromTeX(get("v"))
        set("pictureX", x)
        set("pictureY", y)
        set("inPicture", TRUE)
        ## Save main off-screen device 
        set("savedDevice", get("dvirDevice"))
        ## Create off-screen device for this picture
        pdf(NULL)
        set("dvirDevice", dev.cur())
    } else if (grepl("^end-picture", specialString)) {
        ## Capture the resulting gTree
        gTree <- grid.grab()
        ## Close the off-screen device for this picture
        dev.off()
        ## Restore current device
        set("dvirDevice", get("savedDevice"))
        dev.set(get("dvirDevice"))
        ## "draw" gTree on current device
        grid.draw(gTree)
        set("inPicture", FALSE)
    } else {
        if (get("inPicture")) {
            ## "draw" special off screen 
            drawSpecial(specialString)
        }
    }
}

dvirSpecial <- specialHandler(init=specialInit,
                              metric=specialMetric,
                              grid=gridSpecial)

                           
grid.tikz <- function(...) {
    grid.latex(...,
               preamble=getOption("tikz.preamble"),
               postamble=getOption("dvir.postamble"),
               engine=engine("latex", special=dvirSpecial))
}
