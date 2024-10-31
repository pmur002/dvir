
## Support for 'xcolor' specials in DVI

################################################################################
## specials

colourInit <- function(op) {
    set("colour", NA)
}

## Separate function to aid debugging
updateColour <- function(op) {
    specialString <- paste(blockValue(op$blocks$op.opparams.string),
                           collapse="")
    ## Ignore any other specials
    if (grepl("^color push ", specialString)) {
        token <- strsplit(specialString, " ")[[1]]
        colourspace <- token[3]
        if (colourspace == "gray") {
            colourspec <- as.numeric(token[4])
            colour <- gray(colourspec)
        } else if (colourspace == "rgb") {
            colourspec <- as.numeric(token[4:6])
            colour <- rgb(colourspec[1], colourspec[2], colourspec[3])
        } else {
            warning("Unsupported colourspace - colour not set")
            colour <- NA
        }            
        set("colour", c(colour, get("colour")))
    }
    if (grepl("^color pop", specialString)) {
        set("colour", get("colour")[-1])
    }
}

colourGrob <- function(op) {
    updateColour(op)
}

colourSpecial <- specialHandler(init=colourInit,
                                metric=ignoreSpecial,
                                grob=colourGrob)

################################################################################
## Preambles

colourPreamble <- "\\usepackage{xcolor}"

