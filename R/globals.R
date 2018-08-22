
dvi_grid_globals <- new.env()
get <- function(name) {
    base::get(name, envir=dvi_grid_globals)
}
mget <- function(names) {
    base::mget(names, envir=dvi_grid_globals)
}
set <- function(name, value) {
    assign(name, value, envir=dvi_grid_globals)
}

