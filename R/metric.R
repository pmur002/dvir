
## Run through DVI and extract useful info

updateHoriz <- function(x) {
    right <- get("right")
    if (!is.finite(right) || x > right) {
        set("right", x)
    }
    left <- get("left")
    if (!is.finite(left) || x < left) {
        set("left", x)
    }
}

updateVert <- function(x) {
    top <- get("top")
    if (!is.finite(top) || x < top) {
        set("top", x)
    }
    bottom <- get("bottom")
    if (!is.finite(bottom) || x > bottom) {
        set("bottom", x)
    }
}
    

## set_char_<i>
metric_set_char <- function(op) {
    ## Check position BEFORE char
    updateHoriz(get("h"))
    ## Move to end of char and check position again
    tg <- op_set_char(op)
    updateHoriz(get("h"))
    ## REMEMBER that v measures DOWN
    ## ALSO, v is a baseline for text
    v <- get("v")
    baseline <- get("baseline")
    if (!is.finite(baseline) || v > baseline) {
        set("baseline", v)
    }
    a <- ytoTeX(grobAscent(tg))
    d <- ytoTeX(grobDescent(tg))
    top <- get("top")
    if (!is.finite(top) || v - a < top) {
        set("top", v - a)
    }
    bottom <- get("bottom")
    if (!is.finite(bottom) || v + d > bottom) {
        set("bottom", v + d)
    } 
}
for (i in 0:127) {
    assign(paste0("metric_info_", i), metric_set_char)
}

## set_rule
metric_info_132 <- function(op) {
    ## (h, v) is bottom-left
    h <- get("h")
    v <- get("v")
    updateHoriz(h)
    updateVert(v)
    op_set_rule(op)
    a <- blockValue(op$blocks$op.opparams.a)
    b <- blockValue(op$blocks$op.opparams.b)
    updateVert(v - a)
    updateHoriz(h + b)
}

## put_rule
metric_info_137 <- function(op) {
    ## (h, v) is bottom-left 
    h <- get("h")
    v <- get("v")
    updateHoriz(h)
    updateVert(v)
    op_put_rule(op)
    a <- blockValue(op$blocks$op.opparams.a)
    b <- blockValue(op$blocks$op.opparams.b)
    updateVert(v - a)
    updateHoriz(h + b)
}

## bop
metric_info_139 <- function(op) {
    op_bop(op)
    set("top", Inf)
    set("bottom", -Inf)
    set("baseline", NA)
    set("left", Inf)
    set("right", -Inf)
}

## eop
metric_info_140 <- op_ignore

## push
metric_info_141 <- op_push

## pop
metric_info_142 <- op_pop

## right<i>
metric_info_143 <- op_right
metric_info_144 <- op_right
metric_info_145 <- op_right
metric_info_146 <- op_right

## w<i>
metric_info_147 <- op_w
metric_info_148 <- op_w
metric_info_149 <- op_w
metric_info_150 <- op_w
metric_info_151 <- op_w

## x<i>
metric_info_152 <- op_x
metric_info_153 <- op_x
metric_info_154 <- op_x
metric_info_155 <- op_x
metric_info_156 <- op_x

## down<i>
metric_info_157 <- op_down
metric_info_158 <- op_down
metric_info_159 <- op_down
metric_info_160 <- op_down

## y<i>
metric_info_161 <- op_y
metric_info_162 <- op_y
metric_info_163 <- op_y
metric_info_164 <- op_y
metric_info_165 <- op_y

## z<i>
metric_info_166 <- op_z
metric_info_167 <- op_z
metric_info_168 <- op_z
metric_info_169 <- op_z
metric_info_170 <- op_z

## fnt_num_<i>
for (i in 171:234) {
    assign(paste0("metric_info_", i), op_fnt_num)
}

## xxx<i> (specials)
metric_info_239 <- op_ignore
metric_info_240 <- op_ignore
metric_info_241 <- op_ignore
metric_info_242 <- op_ignore

## font_def<i>
metric_info_243 <- op_font_def

## pre
metric_info_247 <- op_pre

## post
metric_info_248 <- op_ignore

## post_post
metric_info_249 <- op_ignore

readMetricInfo <- function(op) {
    opcode <- blockValue(op$blocks$op.opcode)
    base::get(paste0("metric_info_", opcode))(op)
}

dviMetric <- function(x, device, scale=1, TeX=FALSE) {
    set("device", device)
    set("scale", scale)
    invisible(lapply(x, readMetricInfo))
    info <- list(top=get("top"),
                 bottom=get("bottom"),
                 baseline=get("baseline"),
                 left=get("left"),
                 right=get("right"))
    if (!TeX) {
        info <- lapply(info, fromTeX)
    }
    class(info) <- "DVImetricInfo"
    info
}


