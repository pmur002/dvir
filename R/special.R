
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
    ## Move
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    set("h", xtoTeX(unit(left, "mm") + unit(as.numeric(xy[1]), "pt")))
    set("v", ytoTeX(unit(bottom, "mm") - unit(as.numeric(xy[2]), "pt")))
}

metricLineTo <- function(x) {
    xy <- strsplit(x, ",")[[1]]
    ## Update bbox for start point
    updateHoriz(get("h"))
    updateVert(get("v"))
    ## Move to end point
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    set("h", xtoTeX(unit(left, "mm") + unit(as.numeric(xy[1]), "pt")))
    set("v", ytoTeX(unit(bottom, "mm") - unit(as.numeric(xy[2]), "pt")))
    ## Update bbox for end point
    updateHoriz(get("h"))
    updateVert(get("v"))
}

metricCurveTo <- function(x) {
    xy <- strsplit(x, ",")[[1]]
    ## Update bbox for start point
    updateHoriz(get("h"))
    updateVert(get("v"))
    ## Update bbox to include curve extent
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    startX <- fromTeX(get("h")) - left
    startY <- fromTeX(get("v")) - bottom
    bg <- gridBezier::BezierGrob(x=unit(left, "mm") +
                                     unit(c(startX, xy[c(1, 3, 5)]),
                                          units=c("mm", rep("pt", 3))),
                                 y=unit(bottom, "mm") +
                                     unit(c(startY, xy[c(2, 4, 6)]),
                                          units=c("mm", rep("pt", 3))))
    pts <- gridBezier::BezierPoints(bg)
    l <- min(pts$x)
    r <- max(pts$x)
    b <- min(pts$y)
    t <- max(pts$y)
    updateHoriz(xtoTeX(unit(l, "in")))
    updateHoriz(xtoTeX(unit(r, "in")))
    updateVert(ytoTeX(unit(b, "in")))
    updateVert(ytoTeX(unit(t, "in")))
    ## Move to end of curve
    set("h", xtoTeX(unit(left, "mm") + unit(as.numeric(xy[5]), "pt")))
    set("v", ytoTeX(unit(bottom, "mm") - unit(as.numeric(xy[6]), "pt")))
}

metricPathElement <- function(x) {
    tokens <- strsplit(x, " ")[[1]]
    switch(tokens[1],
           moveto=metricMoveTo(tokens[-1]),
           lineto=metricLineTo(tokens[-1]),
           curveto=metricCurveTo(tokens[-1]),
           stop("unsupported path element"))
}

## Like the drawing, this transform handling is VERY limited at present
## Based on assumption that canvas transforms are only being used
## to locate (and possibly rotate) text (nodes) AND that there will only
## be one such transform in effect at once.
## Furthermore, figuring out the impact of the (possibly rotated) text label
## on the bounding box is too hard (at least at present) so just add
## the transform (label) origin to the bounding box
## (interesting to note that it does not appear that labels are included
##  in the bounding box for TikZ postscript output either !?)
metricTransform <- function(x) {
    tokens <- as.numeric(strsplit(x, ",")[[1]])
    m <- rbind(c(tokens[1], tokens[3], tokens[5]),
               c(tokens[2], tokens[4], tokens[6]))
    trans <- decompose(m)
    if (any(trans$sk != 0) || any(round(trans$sc, 2) != 1))
        warning(paste("Scaling and/or skew in canvas transform;",
                      "this will not end well"))
    ## Transform is relative to picture bottom-left
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    ## Move to location of text
    set("h", xtoTeX(unit(left, "mm") + unit(trans$tr[1], "pt")))
    set("v", ytoTeX(unit(bottom, "mm") + unit(trans$tr[2], "pt")))
    ## Update bbox for location of text
    updateHoriz(get("h"))
    updateVert(get("v"))
}

metricSpecial <- function(x) {
    ## Split by ": " (for paths)
    tokens <- strsplit(gsub(" *$", "", x), ":")[[1]]
    if (length(tokens) == 1) {
        ## Nothing to do ...
        ## APART from transform
        tokens <- strsplit(gsub(" *$", "", tokens), " ")[[1]]
        switch(tokens[1],
               `transform`=metricTransform(tokens[-1]),
               `begin-scope`=,
               `end-scope`=,
               `new-path`=,
               `stroke`={},
               stop("Unsupported TikZ special"))
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
        set("pictureLeft", x)
        set("pictureBottom", y)
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

parseMoveTo <- function(x, i) {
    xy <- strsplit(x, ",")[[1]]
    pathX <- get("pathX")
    pathY <- get("pathY")
    pathX[[i]] <- as.numeric(xy[1])
    pathY[[i]] <- as.numeric(xy[2])
    set("pathX", pathX)
    set("pathY", pathY)
}
parseLineTo <- function(x, i) {
    xy <- strsplit(x, ",")[[1]]
    pathX <- get("pathX")
    pathY <- get("pathY")
    pathX[[i]] <- as.numeric(xy[1])
    pathY[[i]] <- as.numeric(xy[2])
    set("pathX", pathX)
    set("pathY", pathY)
}

parseCurveTo <- function(x, i) {
    xy <- strsplit(x, ",")[[1]]
    pathX <- get("pathX")
    startX <- pathX[[i - 1]][length(pathX[i - 1])]
    pathY <- get("pathY")
    startY <- pathY[[i - 1]][length(pathY[i - 1])]    
    ## Convert Bezier to polyline
    bg <- gridBezier::BezierGrob(x=unit(c(startX, xy[c(1, 3, 5)]), units="pt"),
                                 y=unit(c(startY, xy[c(2, 4, 6)]), units="pt"))
    pts <- gridBezier::BezierPoints(bg)
    pathX[[i]] <- convertX(unit(pts$x[-1], "in"), "pt", valueOnly=TRUE)
    pathY[[i]] <- convertY(unit(pts$y[-1], "in"), "pt", valueOnly=TRUE)
    set("pathX", pathX)
    set("pathY", pathY)
}

drawPathElement <- function(x, i) {
    tokens <- strsplit(x, " ")[[1]]
    switch(tokens[1],
           moveto=parseMoveTo(tokens[-1], i),
           lineto=parseLineTo(tokens[-1], i),
           curveto=parseCurveTo(tokens[-1], i),
           stop("unsupported path element"))
}

drawNewPath <- function() {
    set("pathX", NULL)
    set("pathY", NULL)
}

drawStroke <- function() {
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    pathX <- get("pathX")
    pathY <- get("pathY")
    grid.polyline(x=unit(left, "native") + unit(unlist(pathX), "pt"),
                  y=unit(bottom, "native") + unit(unlist(pathY), "pt"))    
}

## Based on
## https://math.stackexchange.com/questions/13150/extracting-rotation-scale-values-from-2d-transformation-matrix/13165#13165
decompose <- function(m) {
    a <- m[1]
    b <- m[2]
    c <- m[3]
    d <- m[4]
    e <- m[5]
    f <- m[6]
    
    delta <- a*d - b*c

    translation <- c(e, f)
    ## Apply the QR-like decomposition.
    if (a != 0 || b != 0) {
        r <- sqrt(a*a + b*b)
        rotation <- if (b > 0) acos(a/r) else -acos(a/r)
        scale <- c(r, delta/r)
        skew <- c(atan2((a*c + b*d), (r*r)), 0)
    } else if (c != 0 || d != 0) {
        s <- sqrt(c*c + d*d)
        rotation <- pi/2 - if (d > 0) acos(-c/s) else -acos(c/s)
        scale <- c(delta/s, s)
        skew <- c(0, atan2((a*c + b*d), (s*s)))
    } else {
        ## a <- b <- c <- d <- 0
        stop("Invalid transformation matrix")
    }
    list(tr=translation, rot=rotation, sc=scale, sk=skew)
}

## This transform handling is VERY limited at present
## Based on assumption that canvas transforms are only being used
## to locate (and possibly rotate) text (nodes) AND that there will only
## be one such transform in effect at once.
drawTransform <- function(x) {
    tokens <- as.numeric(strsplit(x, ",")[[1]])
    m <- rbind(c(tokens[1], tokens[3], tokens[5]),
               c(tokens[2], tokens[4], tokens[6]))
    trans <- decompose(m)
    if (any(trans$sk != 0) || any(round(trans$sc, 2) != 1))
        warning(paste("Scaling and/or skew in canvas transform;",
                      "this will not end well"))
    ## Transform is relative to picture bottom-left
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    ## Text is drawn relative to current DVI location
    h <- get("h")
    v <- get("v")
    pushViewport(viewport(x=unit(left, "native") + unit(trans$tr[1], "pt"),
                          y=unit(bottom, "native") + unit(trans$tr[2], "pt"),
                          just=c("left", "bottom"),
                          angle=trans$rot,
                          ## Scale so that text drawn at bottom-left
                          xscale=fromTeX(h) + 0:1,
                          yscale=fromTeX(v) + 0:1))
    set("textDepth", get("textDepth") + 1)
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
           col=eval(str2lang(value)),
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
    td <- get("textDepth")
    if (td > 0) {
        popViewport(td)
        set("textDepth", 0)
    }
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
               `transform`=drawTransform(tokens[-1]),
               stop("Unsupported TikZ special"))
    } else {
        ## Path
        n <- length(tokens)
        set("pathX", list(n))
        set("pathY", list(n))
        mapply(drawPathElement, tokens, 1:n)
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
        set("pictureLeft", x)
        set("pictureBottom", y)
        set("textDepth", 0)
        set("inPicture", TRUE)
    } else if (grepl("^end-picture", specialString)) {
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
