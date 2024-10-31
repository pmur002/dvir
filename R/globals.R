
dvi_grid_globals <- new.env()
get <- function(name) {
    base::get0(name, envir=dvi_grid_globals, inherits=FALSE)
}
mget <- function(names) {
    base::mget(names, envir=dvi_grid_globals, inherits=FALSE)
}
set <- function(name, value) {
    assign(name, value, envir=dvi_grid_globals)
}

