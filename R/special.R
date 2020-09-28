
## Handlers for \special operators (xxx<i>)

## These handlers contain a metrics handler and a grob handler
ignoreSpecial <- function(op) {}

specialHandler <- function(init=function() {},
                           metric=ignoreSpecial,
                           grob=ignoreSpecial) {
    structure(list(init=init, metric=metric, grob=grob),
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
specialMetric <- function(op) {
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

handlePathElement <- function(x) {
    tokens <- strsplit(x, " ")[[1]]
    switch(tokens[1],
           moveto=parseMoveTo(tokens[-1]),
           lineto=parseLineTo(tokens[-1]),
           curveto=stop("not yet supported"),
           stop("unsupported path element"))
}

handleNewPath <- function() {
    set("pathX", NULL)
    set("pathY", NULL)
}

handleStroke <- function() {
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

handleBeginScope <- function(x) {
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

handleEndScope <- function() {
    popViewport()
}

handleSpecial <- function(x) {
    ## Split by ": " (for paths)
    tokens <- strsplit(gsub(" *$", "", x), ":")[[1]]
    if (length(tokens) == 1) {
        tokens <- strsplit(gsub(" *$", "", tokens), " ")[[1]]
        switch(tokens[1],
               `begin-scope`=handleBeginScope(tokens[-1]),
               `end-scope`=handleEndScope(),
               `new-path`=handleNewPath(),
               `stroke`=handleStroke(),
               stop("Unsupported TikZ special"))
    } else {
        ## Path
        lapply(tokens, handlePathElement)
        invisible()
    }
}

specialGrob <- function(op) {
    ## <picture> means init buffer
    ## </picture> means parse buffer
    ## Otherwise, accumulate buffer
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
            handleSpecial(specialString)
        }
    }
}

dvirSpecial <- specialHandler(init=specialInit,
                              metric=specialMetric,
                              grob=specialGrob)

                           
grid.tikz <- function(...) {
    grid.latex(...,
               preamble=getOption("tikz.preamble"),
               postamble=getOption("dvir.postamble"),
               engine=engine("latex", special=dvirSpecial))
}
