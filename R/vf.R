
## Code for reading virtual font (.vf) files

## These files are binary, but there is a 'vftopl' tool to convert
## them to human-readable form, BUT there are .vf file variations,
## e.g., for upTeX, that use a variation of the .vf format so
## 'vftopl' does not work

## library(hexView)

## 'opcode', 'int1', etc defined in read.R

## If length is 0, no additional memory read
charOrNull <- function(marker) {
    len <- blockValue(marker)
    if (len > 0) {
        vectorBlock(ASCIIchar, len)
    } else {
        NULL
    }
}

vf_op241 <- markedBlock(marker=uint1,
                        function(marker) {
                            mixedBlock(cc=uint1,
                                       tfm=int3,
                                       dvi=charOrNull(marker))
                        },
                        markerLabel="pl")
                       
vf_op242 <- markedBlock(marker=int4,
                        function(marker) {
                            mixedBlock(cc=int4,
                                       tfm=int4,
                                       dvi=charOrNull(marker))
                        },
                        markerLabel="pl")
                       
vf_op243 <- font_def(1)
vf_op244 <- font_def(2)
vf_op245 <- font_def(3)
vf_op246 <- font_def(4)

vf_op247 <- mixedBlock(i=uint1,
                       comment=markedBlock(marker=int1, charOrNull),
                       cs=int4,
                       ds=int4)

vf_op248 <- NULL

vfopparams <- function(marker) {
    opcode <- blockValue(marker)
    base::get(paste0("vf_op", opcode))
}

vfBlock <- markedBlock(marker=opcode,
                       switch=vfopparams,
                       markerLabel="opcode",
                       blockLabel="opparams")

vfFormat <- memFormat(op=vfBlock)

readVF <- function(f) {
    ## Create a list that is known to be too long
    ops <- vector("list", file.size(f))
    done <- FALSE
    offset <- 0
    length <- 1
    while (!done) {
        op <- readFormat(f, vfFormat, offset=offset)
        ## Just read as far as first 242 or 241
        ## in order to get the font definitions
        if (blockValue(op$blocks$op.opcode) < 243) {
            done <- TRUE
        }
        offset <- offset + op$nbytes
        ops[[length]] <- op
        length <- length + 1
    }
    result <- ops[1:(length - 1)]
    class(result) <- "VF"
    result
}

readVF("/usr/share/texlive/texmf-dist/fonts/vf/uptex-fonts/jis/upjisg-v.vf")


