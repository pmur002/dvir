
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
    xml_attrs(x)
}
parseLineTo <- parseMoveTo

parseLocn <- function(x) {
    switch(xml_name(x),
           moveto=parseMoveTo(x),
           lineto=parseLineTo(x),
           curveto=stop("not yet supported"),
           stop("unsupported path element"))
}

parseStroke <- function(x) {
    xy <- do.call(rbind, lapply(xml_children(x), parseLocn))
    picX <- get("pictureX")
    picY <- get("pictureY")
    polylineGrob(x=unit(picX, "native") + unit(xy[,1], "pt"),
                 y=unit(picY, "native") + unit(xy[,2], "pt"))
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

parseSetting <- function(name, value) {
    switch(name,
           stroke=eval(str2lang(value)),
           fill=eval(str2lang(value)),
           `line-width`=96*parseValueWithUnit(value),
           `line-dash`=parseLineDash(value),
           stop("unsupported setting"))
}

settingNames <- c(col="stroke", fill="fill", lwd="line-width", lty="line-dash")
parseSettingNames <- function(x) {
    index <- match(x, settingNames)
    if (any(is.na(index))) 
        stop(paste0("unsupported setting name (", index[is.na(index)], ")"))
    names(settingNames)[index]
}

## A <g> corresponds to a scope, so create a viewport
## and then parse the content, with the viewport as context
parseG <- function(x) {
    children <- xml_children(x)
    if (length(children)) {
        attrs <- xml_attrs(x)
        attrNames <- names(attrs)
        settings <- mapply(parseSetting, attrNames, attrs, SIMPLIFY=FALSE)
        names(settings) <- parseSettingNames(attrNames)
        vpName <- vpName()
        topvp <- get("viewport")
        vp <- viewport(gp=do.call(gpar, settings),
                       xscale=topvp$xscale, yscale=topvp$yscale,
                       name=vpName)
        gTree(vp=vp,
              children=do.call(gList,
                               lapply(xml_children(x), parseElement)))
    }
}

parseElement <- function(x) {
    switch(xml_name(x),
           g=parseG(x),
           stroke=parseStroke(x),
           stop("Unsupported TikZ special"))
}

parseDVIX <- function(x) {
    xml <- read_xml(x)
    ## Root should be a <picture>
    if (xml_name(xml_root(xml)) != "picture")
        stop("Invalid TikZ special")
    gTree(children=do.call(gList,
                           lapply(xml_children(xml_root(xml)), parseElement)))
}

specialGrob <- function(op) {
    ## <picture> means init buffer
    ## </picture> means parse buffer
    ## Otherwise, accumulate buffer
    specialString <- gsub("dvix:: ", "",
                          paste(blockValue(op$blocks$op.opparams.string),
                                collapse=""))
    if (specialString == "<picture>") {
        x <- fromTeX(get("h"))
        y <- fromTeX(get("v"))
        set("pictureX", x)
        set("pictureY", y)
        set("inPicture", TRUE)
        buffer <- file(open="w+")
        writeLines(specialString, buffer)
        set("specialBuffer", buffer)
    } else if (specialString == "</picture>") {
        set("inPicture", FALSE)
        buffer <- get("specialBuffer")
        writeLines(specialString, buffer)
        specialDVIX <- paste(readLines(buffer), collapse="")
        close(buffer)
        parseDVIX(specialDVIX)
    } else {
        if (get("inPicture")) {
            buffer <- get("specialBuffer")
            writeLines(specialString, buffer)
            set("specialBuffer", buffer)
        }
    }
}

dvirSpecial <- specialHandler(init=specialInit,
                              metric=specialMetric,
                              grob=specialGrob)

                           
