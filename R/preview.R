
################################################################################
## specials

## NOTE that this setting of "baseline" should be orthogonal to
## setting of "baseline" from metric.R as long as the TeX being
## processed does NOT use a TeX/Type1 font.
## (otherwise the "baseline" setting from metric.R will win.)
previewInit <- function(op) {
    set("baseline", NA)
}

previewMetric <- function(op) {
    specialString <- paste(blockValue(op$blocks$op.opparams.string),
                                collapse="")
    ## Ignore any other specials
    if (grepl("^dvir-preview:: ", specialString)) {
        depth <- strsplit(specialString, " ")[[1]][9]
        set("baseline", as.numeric(depth))
    }
}

previewSpecial <- specialHandler(init=previewInit,
                                 metric=previewMetric,
                                 grob=ignoreSpecial)

################################################################################
## Preambles

previewPreamble <-
    c("\\usepackage[active,tightpage,lyx]{preview}",
      "\\makeatletter",
      "\\g@addto@macro\\pr@ship@end{%",
      "\\setbox\\pr@box\\hbox{%",
      "\\special{dvir-preview:: %",
      "Snippet \\number\\pr@snippet\\space%",
      "\\pr@bbadjust\\space%",
      "\\number\\ht\\pr@box\\space%",
      "\\number\\dp\\pr@box\\space%",
      "\\number\\wd\\pr@box%",
      "}%",
      "\\box\\pr@box%",
      "}%",
      "}",
      "\\makeatother")

previewStart <- "\\begin{preview}"

previewEnd <- "\\end{preview}"

