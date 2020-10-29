
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

