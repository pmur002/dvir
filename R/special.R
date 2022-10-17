
## Handlers for \special operators (xxx<i>)

## These handlers contain a metrics handler and a grob handler
ignoreSpecial <- function(op) {}

## Combine multiple specials
combineSpecials <- function(...) {
    specials <- list(...)
    if (!length(specials)) stop("There must be at least one special handler")
    specialHandler(
        init=function() {
            for (s in specials)
                s$init()
        },
        metric=function(op) {
            for (s in specials)
                s$metric(op)
        },
        grob=function(op) {
            for (s in specials)
                s$grob(op)
        })
}

specialHandler <- function(init=function() {},
                           metric=ignoreSpecial,
                           grob=ignoreSpecial) {
    structure(list(init=init, metric=metric, grob=grob),
              class="DVIRspecial")
}

################################################################################
## Totally ignore special 
noSpecial <- specialHandler()

