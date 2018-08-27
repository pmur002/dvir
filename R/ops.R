
set("debug", FALSE)

debugmove <- function() {
    if (get("debug")) {
        grid.move.to(fromTeX(get("h")),
                     unit(1, "npc") - unit(fromTeX(get("v")), "mm"),
                     default.units="mm")
    }
}

debugline <- function(type) {
    col <- switch(type,
                  h=,
                  char=rgb(0,0,1,.5),
                  v=rgb(1,0,0,.5),
                  pop=rgb(0,1,0,.5))
    if (get("debug")) {
        grid.line.to(fromTeX(get("h")),
                     unit(1, "npc") - unit(fromTeX(get("v")), "mm"),
                     default.units="mm",
                     gp=gpar(col=col))
        grid.circle(fromTeX(get("h")),
                    unit(1, "npc") - unit(fromTeX(get("v")), "mm"),
                    r=1,
                    default.units="mm",
                    gp=gpar(col=NA, fill=col))
    }
}

## DVI operations that can be shared by DVI parsers

op_ignore <- function(op) { }

## set_char_<i>
op_set_char <- function(op) {
    debugmove()
    x <- unit(fromTeX(get("h")), "native")
    y <- unit(fromTeX(get("v")), "native")
    fonts <- get("fonts")
    f <- get("f")
    char <- getChar(op$blocks$op.opcode$fileRaw,
                    fonts[[f]]$postscriptname,
                    get("device"))
    family <- fontFamily(fonts[[f]], char, get("device"))
    ## "native" rather than "mm" because the grobs will be drawn
    ## within a viewport with scales based on "mm" dimensions of
    ## the entire DVI
    tg <- textGrob(char, x, y, just=c("left", "bottom"),
                   gp=gpar(fontfamily=family,
                           fontsize=fonts[[f]]$size,
                           ## TODO
                           ## Simply scaling text up like this is NOT
                           ## right because it does not respect the
                           ## font *design size*
                           ## (see http://makingtexwork.sourceforge.net/mtw/ch05.html
                           ##  section "The Issue of Size")
                           cex=get("scale")))
    ## grobWidth() works with text font size, NOT text location (x, y)
    ## so "native" for (x, y) should be ok
    set("h", get("h") + charWidth(op, fonts, f))
    debugline("char")
    ## Return text grob
    tg
}

## set_rule
op_set_rule <- function(op) {
    b <- blockValue(op$blocks$op.opparams.b)
    set("h", get("h") + b)
}

## put_rule
op_put_rule <- function(op) { }    

## bop
op_bop <- function(op) {
    set("h", 0)
    set("v", 0)
    set("w", 0)
    set("x", 0)
    set("y", 0)
    set("z", 0)
    set("stack", list())
    set("i", 0)
    set("f", NA)
}

## push
op_push <- function(op) {
    i <- get("i")
    stack <- get("stack")
    stack[[i + 1]] <- mget(c("h", "v", "w", "x", "y", "z"))
    set("i", i + 1)
    set("stack", stack)
}

## pop
op_pop <- function(op) {
    debugmove()
    i <- get("i")
    stack <- get("stack")
    values <- stack[[i]]
    mapply(set, names(values), values)
    set("i", i - 1)
    set("stack", stack[-i])
    debugline("pop")
}

## right<i>
op_right <- function(op) {
    debugmove()
    b <- blockValue(op$blocks$op.opparams)
    set("h", get("h") + b)
    debugline("h")
}

## w<i>
op_w <- function(op) {
    debugmove()
    b <- op$blocks$op.opparams
    if (!is.null(b)) {
        set("w", blockValue(b))
    }
    set("h", get("h") + get("w"))
    debugline("h")
}

## x<i>
op_x <- function(op) {
    debugmove()
    b <- op$blocks$op.opparams
    if (!is.null(b)) {
        set("x", blockValue(b))
    }
    set("h", get("h") + get("x"))
    debugline("h")
}

## down<i>
op_down <- function(op) {
    debugmove()
    a <- blockValue(op$blocks$op.opparams)
    set("v", get("v") + a)
    debugline("v")
}

## y<i>
op_y <- function(op) {
    debugmove()
    a <- op$blocks$op.opparams
    if (!is.null(a)) {
        set("y", blockValue(a))
    }
    set("v", get("v") + get("y"))
    debugline("v")
}

## z<i>
op_z <- function(op) {
    debugmove()
    a <- op$blocks$op.opparams
    if (!is.null(a)) {
        set("z", blockValue(a))
    }
    set("v", get("v") + get("z"))
    debugline("v")
}

## fnt_num_<i>
op_fnt_num <- function(op) {
    f <- blockValue(op$blocks$op.opcode) - 171 + 1 ## + 1 for 1-based indexing
    set("f", f)
}

## font_def<i>
op_font_def <- function(op) {
    fontname <- paste(blockValue(op$blocks$op.opparams.fontname.name),
                      collapse="")
    fontdef <- defineFont(fontname, get("device"))
    fontnum <- blockValue(op$blocks$op.opparams.k) + 1
    scale <- blockValue(op$blocks$op.opparams.s)
    design <- blockValue(op$blocks$op.opparams.d)
    mag <- get("mag")
    fonts <- get("fonts")
    fonts[[fontnum]] <- c(list(name=fontname),
                              fontdef)
    fonts[[fontnum]]$size <- fontdef$size*mag*scale/(1000*design)
    set("fonts", fonts)
}

## pre
op_pre <- function(op) {
    num <- blockValue(op$blocks$op.opparams.num)
    den <- blockValue(op$blocks$op.opparams.den)
    mag <- blockValue(op$blocks$op.opparams.mag)
    set("num", num)
    set("den", den)
    set("mag", mag)
    set("fonts", vector("list", 255))
}
