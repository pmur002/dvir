
library(grid)

################################################################################
## Traverse DVI information in memory and draw 'grid' representation

## set_char_<i>
for (i in 0:127) {
    assign(paste0("grid_op_", i), op_set_char)
}

## set_rule
gridRule <- function(op) {
    a <- blockValue(op$blocks$op.opparams.a)
    b <- blockValue(op$blocks$op.opparams.b)
    x <- unit(fromTeX(get("h")), "native")
    y <- unit(fromTeX(get("v")), "native")
    width <- unit(fromTeX(b), "native")
    height <- unit(fromTeX(a), "native")
    rectGrob(x, y, width, height, just=c("left", "bottom"),
             gp=gpar(col=NA, fill="black"))
}

grid_op_132 <- function(op) {
    gridRule(op)
    op_set_rule(op)
}

## put_rule
grid_op_137 <- function(op) {
    gridRule(op)
}

## bop
grid_op_139 <- op_bop

## eop
grid_op_140 <- op_ignore

## push
grid_op_141 <- op_push

## pop
grid_op_142 <- op_pop

## right<i>
grid_op_143 <- op_right
grid_op_144 <- op_right
grid_op_145 <- op_right
grid_op_146 <- op_right

## w<i>
grid_op_147 <- op_w
grid_op_148 <- op_w
grid_op_149 <- op_w
grid_op_150 <- op_w
grid_op_151 <- op_w

## x<i>
grid_op_152 <- op_x
grid_op_153 <- op_x
grid_op_154 <- op_x
grid_op_155 <- op_x
grid_op_156 <- op_x

## down<i>
grid_op_157 <- op_down
grid_op_158 <- op_down
grid_op_159 <- op_down
grid_op_160 <- op_down

## y<i>
grid_op_161 <- op_y
grid_op_162 <- op_y
grid_op_163 <- op_y
grid_op_164 <- op_y
grid_op_165 <- op_y

## fnt_num_<i>
for (i in 171:234) {
    assign(paste0("grid_op_", i), op_fnt_num)
}

## xxx<i> (specials)
grid_op_239 <- op_ignore
grid_op_240 <- op_ignore
grid_op_241 <- op_ignore
grid_op_242 <- op_ignore

## font_def<i>
grid_op_243 <- op_font_def

## pre
grid_op_247 <- op_pre

## post
grid_op_248 <- op_ignore

## post_post
grid_op_249 <- op_ignore

op2grid <- function(op) {
    opcode <- blockValue(op$blocks$op.opcode)
    base::get(paste0("grid_op_", opcode))(op)
}

dvigrid <- function(x, device, scale=1) {
    set("device", device)
    set("scale", scale)
    invisible(lapply(x, op2grid))
}

################################################################################
## User interface

dviGrob <- function(dvi, x, y, default.units, rot, device, gp, name) {
    UseMethod("dviGrob")
}

dviGrob.character <- function(dvi,
                              x=0.5, y=0.5,
                              default.units="npc", just="centre",
                              rot=0,
                              device=names(dev.cur()),
                              name=NULL) {
    dviGrob(readDVI(dvi), x, y, default.units, just, rot, device, name)
}

dviGrob.DVI <- function(dvi,
                        x=0.5, y=0.5,
                        default.units="npc", just="centre",
                        rot=0,
                        device=names(dev.cur()),
                        name=NULL) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (device == "null device") {
        dev.new()
        device <- names(dev.cur())
    }
    fonts <- dviFonts(dvi, device)
    metrics <- dviMetric(dvi, device)
    vp <- viewport(x, y,
                   width=unit(metrics$right - metrics$left, "mm"),
                   ## Down is positive in DVI
                   height=unit(metrics$bottom - metrics$top, "mm"),
                   just=just, angle=rot,
                   xscale=c(metrics$left, metrics$right),
                   yscale=c(metrics$bottom, metrics$top))
    grobs <- dvigrid(dvi, device)
    children <- do.call(gList, grobs[sapply(grobs, is.grob)])
    gTree(children=children, fonts=fonts, vp=vp, name=name, cl="DVIgrob")
}

grid.dvi <- function(...) {
    grid.draw(dviGrob(...))
}

## Very simplistic for now
## Needs flexibility in terms of LaTeX preable/postamble, TeX engine, ...
latexGrob <- function(tex,
                      x=0.5, y=0.5,
                      default.units="npc", just="centre",
                      rot=0,
                      device=names(dev.cur()),
                      name=NULL) {
    texFile <- tempfile(fileext=".tex")
    dviFile <- gsub("[.]tex", ".dvi", texFile)
    writeLines(c("\\documentclass[12pt]{standalone}",
                 "\\begin{document}",
                 tex,
                 "\\end{document}"),
               texFile)
    system(paste0("latex -output-directory=", tempdir(), " ", texFile))
    dvi <- readDVI(dviFile)
    dviGrob(dvi, x, y, default.units, just, rot, device, name)
}
    
grid.latex <- function(...) {
    grid.draw(latexGrob(...))
}
                       
## Helper for embedding fonts in postscript() or pdf() output
fontPaths <- function(x) {
    if (!inherits(x, "DVIgrob"))
        stop("'x' must be a DVIgrob")
    if (x$fonts$device %in% c("postscript", "pdf")) {
        paths <- sapply(x$fonts$fonts,
                        function(f) {
                            if (is.null(f$pfb)) {
                                ""
                            } else {
                                dirname(f$pfb)
                            }
                        })
        paste(paths[nchar(paths) > 0], collapse=":")
    } else if (grepl("cairo", x$fonts$device)) {
        ""
    } else {
        ""
    }
}
