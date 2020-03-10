
library(grid)

################################################################################
## Traverse DVI information in memory and draw 'grid' representation

## set_char_<i>
for (i in 0:127) {
    assign(paste0("grid_op_", i), op_set_char)
}

grid_op_128 <- op_set
grid_op_129 <- op_set
grid_op_130 <- op_set
grid_op_131 <- op_set

## set_rule
## NOTE that we need to do a "rule_pixels" calculation here
## http://ftp.cs.stanford.edu/tex/dist/texware/dvitype.web

## Below a certain thickness, draw a line segment rather
## than filling a rectangle
## This will allow, e.g., raster devices to antialias the result
## (which should look much better for thin rules)

gridRule <- function(op) {
    a <- blockValue(op$blocks$op.opparams.a)
    b <- blockValue(op$blocks$op.opparams.b)
    device <- get("device")
    if (a > 0 && b > 0) {
        x <- fromTeX(get("h"))
        y <- fromTeX(get("v"))
        width <- fromTeX(b)
        height <- fromTeX(a)
        ## Below lwd=1, draw a line
        if (width < 25.4/72) {
            segmentsGrob(x + width/2,
                         y,
                         x + width/2,
                         y + height,
                         default.units="native",
                         gp=gpar(lwd=72*width/25.4,
                                 lineend="butt"))
        } else if (height < 25.4/72) {
            segmentsGrob(x,
                         y + height/2,
                         x + width,
                         y + height/2,
                         default.units="native",
                         gp=gpar(lwd=72*height/25.4,
                                 lineend="butt"))
        } else {
            rectGrob(x, y, width, height, default.units="native",
                     just=c("left", "bottom"),
                     gp=gpar(col=NA, fill="black"))
        }
    } else {
        NULL
    }
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

## z<i>
grid_op_166 <- op_z
grid_op_167 <- op_z
grid_op_168 <- op_z
grid_op_169 <- op_z
grid_op_170 <- op_z

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

dvigrid <- function(x, device, engine, scale=1) {
    set("device", device)
    set("engine", engine)
    set("scale", scale)
    invisible(lapply(x, op2grid))
}

################################################################################
## User interface

dviGrob <- function(dvi, ...) {
    UseMethod("dviGrob")
}

dviGrob.character <- function(dvi,
                              x=0.5, y=0.5,
                              default.units="npc", just="centre",
                              rot=0,
                              device=names(dev.cur()),
                              name=NULL, ...) {
    dviGrob(readDVI(dvi), x, y, default.units, just, rot, device, name)
}

dviGrob.DVI <- function(dvi,
                        x=0.5, y=0.5,
                        default.units="npc", just="centre",
                        rot=0,
                        device=names(dev.cur()),
                        name=NULL,
                        engine=texEngine,
                        ...) {
    if (!is.unit(x))
        x <- unit(x, default.units)
    if (!is.unit(y))
        y <- unit(y, default.units)
    if (device == "null device") {
        dev.new()
        device <- names(dev.cur())
    }
    fonts <- dviFonts(dvi, device, engine)
    metrics <- dviMetric(dvi, device, engine)
    vp <- viewport(x, y,
                   width=unit(metrics$right - metrics$left, "mm"),
                   ## Down is positive in DVI
                   height=unit(metrics$bottom - metrics$top, "mm"),
                   just=just, angle=rot,
                   xscale=c(metrics$left, metrics$right),
                   yscale=c(metrics$bottom, metrics$top),
                   name="dvi.vp")
    grobs <- dvigrid(dvi, device, engine)
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
                      name=NULL,
                      preamble=getOption("dvir.preamble"),
                      postamble=getOption("dvir.postamble"),
                      engine=texEngine,
                      tinytex=FALSE) {
    haveTinyTeX <- tinytex && requireNamespace("tinytex", quietly=TRUE)
    if (!haveTinyTeX) {
        haveLaTeX <- nchar(Sys.which("latex"))
        if (!haveLaTeX) {
            stop("LaTeX not found")
        }
    }
    texFile <- tempfile(fileext=".tex")
    dviFile <- gsub("[.]tex", ".dvi", texFile)
    writeLines(c(preamble,
                 tex,
                 postamble),
               texFile)
    if (haveTinyTeX) {
        tinytex::latexmk(texFile, engine=engine$engine,
                         engine_args=engine$options)
    } else {
        system(paste0(engine$engine, " ", engine$options,
                      " -output-directory=", tempdir(), " ", texFile))
    }
    dvi <- readDVI(dviFile)
    dviGrob(dvi, x, y, default.units, just, rot, device, name, engine)
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
        paste(unique(paths[nchar(paths) > 0]), collapse=":")
    } else if (grepl("cairo", x$fonts$device)) {
        ""
    } else {
        ""
    }
}
