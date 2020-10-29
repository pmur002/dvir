
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
    xy <- as.numeric(strsplit(x, ",")[[1]])
    ## Apply current transform
    mt <- get("metricTransform")[[1]]
    xy <- mt %*% c(xy, 1)
    ## Move
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    set("pictureX", xtoTeX(unit(left, "mm") + unit(xy[1], "pt")))
    set("pictureY", ytoTeX(unit(bottom, "mm") - unit(xy[2], "pt")))
}

metricLineTo <- function(x) {
    xy <- as.numeric(strsplit(x, ",")[[1]])
    ## Apply current transform
    mt <- get("metricTransform")[[1]]
    xy <- mt %*% c(xy, 1)
    ## Update bbox for start point
    updateHoriz(get("pictureX"))
    updateVert(get("pictureY"))
    ## Move to end point
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    set("pictureX", xtoTeX(unit(left, "mm") + unit(xy[1], "pt")))
    set("pictureY", ytoTeX(unit(bottom, "mm") - unit(xy[2], "pt")))
    ## Update bbox for end point
    updateHoriz(get("pictureX"))
    updateVert(get("pictureY"))
}

metricCurveTo <- function(x) {
    xy <- as.numeric(strsplit(x, ",")[[1]])
    ## Apply current transform
    mt <- get("metricTransform")[[1]]
    xy <- (mt %*% rbind(matrix(xy, nrow=2), 1))[-3,]
    ## Update bbox for start point
    updateHoriz(get("pictureX"))
    updateVert(get("pictureY"))
    ## Update bbox to include curve extent
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    startX <- fromTeX(get("pictureX")) - left
    startY <- bottom - fromTeX(get("pictureY"))
    bg <- gridBezier::BezierGrob(x=unit(left, "mm") +
                                     unit(c(startX, xy[c(1, 3, 5)]),
                                          units=c("mm", rep("pt", 3))),
                                 y=unit(bottom, "mm") -
                                     unit(c(startY, xy[c(2, 4, 6)]),
                                          units=c("mm", rep("pt", 3))))
    pts <- gridBezier::BezierPoints(bg)
    l <- min(pts$x)
    r <- max(pts$x)
    b <- max(pts$y)
    t <- min(pts$y)
    updateHoriz(xtoTeX(unit(l, "in")))
    updateHoriz(xtoTeX(unit(r, "in")))
    updateVert(ytoTeX(unit(b, "in")))
    updateVert(ytoTeX(unit(t, "in")))
    ## Move to end of curve
    set("pictureX", xtoTeX(unit(left, "mm") + unit(as.numeric(xy[5]), "pt")))
    set("pictureY", ytoTeX(unit(bottom, "mm") - unit(as.numeric(xy[6]), "pt")))
}

metricPathElement <- function(x) {
    tokens <- strsplit(x, " ")[[1]]
    switch(tokens[1],
           moveto=metricMoveTo(tokens[-1]),
           lineto=metricLineTo(tokens[-1]),
           curveto=metricCurveTo(tokens[-1]),
           close={},
           stop("unsupported path element"))
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

## This transform handling is limited
## It only handles translation and rotation
## It cannot handle a transform that scales or skews 
## Furthermore, figuring out the impact of (possibly rotated) text 
## on the bounding box is too hard (at least at present) so we just add
## the transform (label) origin to the bounding box
## (interesting to note that it does not appear that labels are included
##  in the bounding box for TikZ postscript output either !?)
metricTransform <- function(x) {
    tokens <- as.numeric(strsplit(x, ",")[[1]])
    m <- rbind(c(tokens[1], tokens[3], tokens[5]),
               c(tokens[2], tokens[4], tokens[6]))
    ## Update current transform (for picture x/y)
    mt <- get("metricTransform")
    mt[[1]] <- rbind(m, c(0, 0, 1)) %*% mt[[1]]
    set("metricTransform", mt)
    ## TEMPORARILY set h/v even though we are not drawing so that
    ## metric_set_char updates bbox correctly
    ## NOTE that this will NOT take into account rotation of text
    trans <- decompose(m)
    if (any(trans$sk != 0) || any(round(trans$sc, 2) != 1))
        warning(paste("Scaling and/or skew in canvas transform;",
                      "this will not end well"))
    ## Transform is relative to picture bottom-left
    left <- get("pictureLeft")
    bottom <- get("pictureBottom")
    ## Move to location of text
    textX <- xtoTeX(unit(left, "mm") + unit(trans$tr[1], "pt"))
    textY <- ytoTeX(unit(bottom, "mm") - unit(trans$tr[2], "pt"))
    set("h", textX)
    set("v", textY)
}

metricBeginScope <- function() {
    ## Push current transform
    mt <- get("metricTransform")
    set("metricTransform", c(mt[1], mt))
}

metricEndScope <- function() {
    ## Pop current transform
    mt <- get("metricTransform")
    set("metricTransform", mt[-1])
}

measureSpecial <- function(x) {
    ## Ignore "blanks"
    if (grepl("^ *$", x)) return()
    ## Split by ": " (for paths)
    tokens <- strsplit(gsub("^ *| *$", "", x), ":")[[1]]
    if (length(tokens) == 1) {
        ## Nothing to do ...
        ## APART from transform
        tokens <- strsplit(gsub(" *$", "", tokens), " ")[[1]]
        switch(tokens[1],
               `begin-scope`=metricBeginScope(),
               `transform`=metricTransform(tokens[-1]),
               `end-scope`=metricEndScope(),
               `new-path`=,
               `stroke`=,
               `fill`=,
               `fill-stroke`={},
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
        h <- get("h")
        v <- get("v")
        x <- fromTeX(h)
        y <- fromTeX(v)
        set("pictureLeft", x)
        set("pictureBottom", y)
        set("pictureX", h)
        set("pictureY", v)
        set("savedH", h)
        set("savedV", v)
        set("metricTransform", list(diag(3)))
        set("inPicture", TRUE)
    } else if (grepl("^end-picture", specialString)) {
        set("h", get("savedH"))
        set("v", get("savedV"))        
        set("inPicture", FALSE)
    } else {
        if (get("inPicture")) {
            ## Output may be multiple specials from
            ## "protocolled" (recorded) output, so split first by ";"
            specials <- strsplit(specialString, ";")[[1]]
            lapply(specials, measureSpecial)
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
    sub <- get("subPath") + 1
    pathX <- get("pathX")
    pathY <- get("pathY")
    pathX[[sub]][[i]] <- as.numeric(xy[1])
    pathY[[sub]][[i]] <- as.numeric(xy[2])
    set("pathX", pathX)
    set("pathY", pathY)
    set("subPath", sub)
}
parseLineTo <- function(x, i) {
    xy <- strsplit(x, ",")[[1]]
    sub <- get("subPath")
    pathX <- get("pathX")
    pathY <- get("pathY")
    pathX[[sub]][[i]] <- as.numeric(xy[1])
    pathY[[sub]][[i]] <- as.numeric(xy[2])
    set("pathX", pathX)
    set("pathY", pathY)
}

parseCurveTo <- function(x, i) {
    xy <- strsplit(x, ",")[[1]]
    sub <- get("subPath")
    pathX <- get("pathX")
    startX <- pathX[[sub]][[i - 1]][length(pathX[[sub]][[i - 1]])]
    pathY <- get("pathY")
    startY <- pathY[[sub]][[i - 1]][length(pathY[[sub]][[i - 1]])]    
    ## Convert Bezier to polyline
    bg <- gridBezier::BezierGrob(x=unit(c(startX, xy[c(1, 3, 5)]), units="pt"),
                                 y=unit(c(startY, xy[c(2, 4, 6)]), units="pt"))
    pts <- gridBezier::BezierPoints(bg)
    pathX[[sub]][[i]] <- convertX(unit(pts$x[-1], "in"), "pt", valueOnly=TRUE)
    pathY[[sub]][[i]] <- convertY(unit(pts$y[-1], "in"), "pt", valueOnly=TRUE)
    set("pathX", pathX)
    set("pathY", pathY)
}

parseClose <- function(i) {
    ## Start new subPath
    sub <- get("subPath") + 1
    ## Mark old subPath as closed
    closed <- get("pathClosed")
    closed[sub - 1] <- TRUE
    set("pathClosed", closed)
    ## New path begins at start point of previous subPath
    ## (this may immediately get superceded by moveto, BUT OTOH it may NOT)
    pathX <- get("pathX")
    pathY <- get("pathY")
    pathX[[sub]][[i]] <- pathX[[sub - 1]][[1]]
    pathY[[sub]][[i]] <- pathY[[sub - 1]][[1]]
    set("pathX", pathX)
    set("pathY", pathY)
    set("subPath", sub)
}

drawPathElement <- function(x, i) {
    tokens <- strsplit(x, " ")[[1]]
    if (i == 1 && tokens[1] != "moveto") {
        stop("Invalid path (must begin with moveto)")
    }
    switch(tokens[1],
           moveto=parseMoveTo(tokens[-1], i),
           lineto=parseLineTo(tokens[-1], i),
           curveto=parseCurveTo(tokens[-1], i),
           close=parseClose(i),
           stop("unsupported path element"))
}

drawNewPath <- function(x) {
    set("pathX", NULL)
    set("pathY", NULL)
    drawViewport(x)
}

drawStroke <- function() {
    pathX <- get("pathX")
    pathY <- get("pathY")
    closed <- get("pathClosed")
    mapply(function(px, py, cl) {
               if (length(unlist(px)) > 1) {
                   if (cl) {
                       grid.path(x=unit(unlist(px), "pt"),
                                 y=unit(unlist(py), "pt"),
                                 gp=gpar(fill=NA))
                   } else {
                       grid.polyline(x=unit(unlist(px), "pt"),
                                     y=unit(unlist(py), "pt"))
                   }
               }
           },
           pathX, pathY, closed)
    ## Undo new-path viewport
    popViewport()
}

drawFill <- function() {
    pathX <- get("pathX")
    pathY <- get("pathY")
    mapply(function(px, py) {
               if (length(unlist(px)) > 1) {
                   grid.path(x=unit(unlist(px), "pt"),
                             y=unit(unlist(py), "pt"),
                             gp=gpar(col=NA))
               }
           },
           pathX, pathY)
    ## Undo new-path viewport
    popViewport()
}

drawFillStroke <- function() {
    pathX <- get("pathX")
    pathY <- get("pathY")
    closed <- get("pathClosed")
    mapply(function(px, py) {
               if (length(unlist(px)) > 1) {
                   grid.path(x=unit(unlist(px), "pt"),
                             y=unit(unlist(py), "pt"),
                             gp=gpar(col=NA))
               }
           },
           pathX, pathY)
    mapply(function(px, py, cl) {
               if (length(unlist(px)) > 1) {
                   if (cl) {
                       grid.path(x=unit(unlist(px), "pt"),
                                 y=unit(unlist(py), "pt"),
                                 gp=gpar(fill=NA))
                   } else {
                       grid.polyline(x=unit(unlist(px), "pt"),
                                     y=unit(unlist(py), "pt"))
                   }
               }
           },
           pathX, pathY, closed)
    ## Undo new-path viewport
    popViewport()
}

## This transform handling is limited
## It only handles translation and rotation
## It cannot handle a transform that scales or skews 
drawTransform <- function(x) {
    tokens <- as.numeric(strsplit(x, ",")[[1]])
    m <- rbind(c(tokens[1], tokens[3], tokens[5]),
               c(tokens[2], tokens[4], tokens[6]))
    trans <- decompose(m)
    if (any(trans$sk != 0) || any(round(trans$sc, 2) != 1))
        warning(paste("Scaling and/or skew in canvas transform;",
                      "this will not end well"))
    cv <- current.viewport()
    pushViewport(viewport(x=unit(trans$tr[1], "pt"),
                          y=unit(trans$tr[2], "pt"),
                          just=c("left", "bottom"),
                          angle=trans$rot/pi*180,
                          ## Scale so that text drawn at bottom-left
                          ## (and subsequent text drawn alongside)
                          xscale=c(0, diff(cv$xscale)),
                          yscale=c(0, diff(cv$yscale))))
    if (get("debug"))
        grid.rect(gp=gpar(col="grey", fill=NA))
    ## Update transform depth
    td <- get("transformDepth")
    td[1] <- td[1] + 1
    set("transformDepth", td)
    ## Update DVI location for text (drawn within this transform)
    set("h", 0)
    set("v", 0)
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
           lineend=value,
           linejoin=value,
           `stroke-opacity`=as.numeric(value),
           stop("unsupported setting"))
}

handleOpacity <- function(x) {
    if ("stroke-opacity" %in% names(x)) {
        if ("col" %in% names(x)) {
            x$col <- adjustcolor(x$col, alpha=x$"stroke-opacity")
        }
        x$"stroke-opacity" <- NULL
    }
    if ("fill-opacity" %in% names(x)) {
        if ("fill" %in% names(x)) {
            x$fill <- adjustcolor(x$fill, alpha=x$"fill-opacity")
        }
        x$"fill-opacity" <- NULL
    }
    x
}

drawViewport <- function(x) {
    if (length(x) == 0) {
        gp <- gpar()
    } else {
        tokens <- strsplit(x, "=")
        names <- sapply(tokens, "[", 1)
        values <- lapply(tokens, parseSetting)
        names(values) <- names
        gp <- do.call(gpar, handleOpacity(values))
    }
    cv <- current.viewport()
    vp <- viewport(gp=gp,
                   xscale=cv$xscale, yscale=cv$yscale,
                   name=vpName())
    pushViewport(vp)
}

drawBeginScope <- function(x) {
    ## Push scope viewport
    drawViewport(x)
    ## Push transform depth
    td <- get("transformDepth")
    set("transformDepth", c(0, td))
}

drawEndScope <- function() {
    ## Pop transform depth
    td <- get("transformDepth")
    if (td[1] > 0) {
        ## Pop transform viewports
        popViewport(td[1])
    }
    set("transformDepth", td[-1])
    ## Pop scope viewport
    popViewport()
}

drawSpecial <- function(x) {
    ## Ignore "blanks"
    if (grepl("^ *$", x)) return()
    ## Split by ": " (for paths)
    tokens <- strsplit(gsub("^ *| *$", "", x), ":")[[1]]
    if (length(tokens) == 0) {
        warning("Empty special")
    } else if (length(tokens) == 1) {
        tokens <- strsplit(gsub(" *$", "", tokens), " ")[[1]]
        switch(tokens[1],
               `begin-scope`=drawBeginScope(tokens[-1]),
               `end-scope`=drawEndScope(),
               `new-path`=drawNewPath(tokens[-1]),
               `stroke`=drawStroke(),
               `fill`=drawFill(),
               `fill-stroke`=drawFillStroke(), 
               `transform`=drawTransform(tokens[-1]),
               stop("Unsupported TikZ special"))
    } else {
        ## Path
        n <- length(tokens)
        ## Count number of moveto's and close's
        nsub <- length(grep("moveto|close", tokens))
        ## Create subpath for each moveto and close
        set("subPath", 0)
        ## (record path element i in component i of relevant subpath)
        set("pathX", lapply(1:nsub, function(i) vector("list", n)))
        set("pathY", lapply(1:nsub, function(i) vector("list", n)))
        ## Is each subpath closed ? (FALSE by default)
        set("pathClosed", logical(nsub))
        mapply(drawPathElement, tokens, 1:n)
        invisible()
    }
}

gridSpecial <- function(op) {
    specialString <- gsub("dvir:: ", "",
                          paste(blockValue(op$blocks$op.opparams.string),
                                collapse=""))
    if (grepl("^begin-picture", specialString)) {
        h <- get("h")
        v <- get("v")
        set("savedH", h)
        set("savedV", v)
        x <- fromTeX(h)
        y <- fromTeX(v)
        cv <- current.viewport()
        pushViewport(viewport(unit(x, "native"), unit(y, "native"),
                              just=c("left", "bottom"),
                              xscale=cv$xscale, yscale=cv$yscale))
        if (get("debug"))
            grid.rect(gp=gpar(col="grey", fill=NA))
        set("transformDepth", 0)
        set("inPicture", TRUE)
    } else if (grepl("^end-picture", specialString)) {
        popViewport()
        set("h", get("savedH"))
        set("v", get("savedV"))        
        set("inPicture", FALSE)
    } else {
        if (get("inPicture")) {
            ## "draw" special off screen
            ## Output may be multiple specials from
            ## "protocolled" (recorded) output, so split first by ";"
            specials <- strsplit(specialString, ";")[[1]]
            lapply(specials, drawSpecial)
        }
    }
}

tikzSpecial <- specialHandler(init=specialInit,
                              metric=specialMetric,
                              grid=gridSpecial)

################################################################################
## User interface

tikzPreamble <- function(packages=NULL) {
    if (!is.null(packages)) {
        usepackages <- paste0("\\usetikzlibrary{", packages, "}",
                              collapse="\n")
    } else {
        usepackages <- NULL
    }
    paste("\\documentclass[12pt]{standalone}",
          paste0("\\def\\pgfsysdriver{",
                 system.file("tikz", "pgfsys-dvir.def",
                             package="dvir"),
                 "}"),
          "\\usepackage{tikz}",
          usepackages,
          "\\begin{document}",
          sep="\n")
}

tikzpicturePreamble <- function(packages=NULL) {
    paste(tikzPreamble(packages),
          "\\begin{tikzpicture}",
          sep="\n")
}

tikzGrob <- function(tex, ...,
                     preamble=getOption("tikz.preamble"),
                     postamble=getOption("dvir.postamble"),
                     engine=TeXengine("latex", special=tikzSpecial)) {
    latexGrob(tex, ...,
              preamble=preamble, postamble=postamble, engine=engine)
}

grid.tikz <- function(...) {
    grid.draw(tikzGrob(...))
}

tikzpictureGrob <- function(tex, ...,
                            preamble=getOption("tikzpicture.preamble"),
                            postamble=getOption("tikzpicture.postamble"),
                            engine=TeXengine("latex", special=tikzSpecial)) {
    latexGrob(tex, ...,
              preamble=preamble, postamble=postamble, engine=engine)
}

grid.tikzpicture <- function(...) {
    grid.draw(tikzpictureGrob(...))
}
