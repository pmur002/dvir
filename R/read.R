
library(hexView)

## The file consists of operations
## Each operation starts with a one-byte opcode

opcode <- atomicBlock("int", size=1, signed=FALSE)

int1 <- integer1
int2 <- atomicBlock("int", size=2, endian="big")
int3 <- integer3(endian="big")
int4 <- atomicBlock("int", size=4, endian="big")

uint1 <- atomicBlock("int", size=1, signed=FALSE)
uint2 <- atomicBlock("int", size=2, endian="big", signed=FALSE)

################################################################################
## operation parameters

## set_char_<i>
for (i in 0:127) {
    assign(paste0("op", i), NULL)
}

## set<i>
op128 <- int1
op129 <- int2
op130 <- int3
op131 <- int4
    
## set_rule
op132 <- mixedBlock(a=int4,
                    b=int4)

## put<i>
op133 <- int1
op134 <- int2
op135 <- int3
op136 <- int4

## put_rule
op137 <- mixedBlock(a=int4,
                    b=int4)

## nop
op138 <- NULL

## bop
op139 <- mixedBlock(counters=vectorBlock(int4, 10),
                    p=int4)

## eop
op140 <- NULL

## push
op141 <- NULL

## pop
op142 <- NULL

## right<i>
op143 <- int1
op144 <- int2
op145 <- int3
op146 <- int4

## w<i>
op147 <- NULL
op148 <- int1
op149 <- int2
op150 <- int3
op151 <- int4

## x<i>
op152 <- NULL
op153 <- int1
op154 <- int2
op155 <- int3
op156 <- int4

## down<i>
op157 <- int1
op158 <- int2
op159 <- int3
op160 <- int4

## y<i>
op161 <- NULL
op162 <- int1
op163 <- int2
op164 <- int3
op165 <- int4

## z<i>
op166 <- NULL
op167 <- int1
op168 <- int2
op169 <- int3
op170 <- int4

## fnt_num_<i>
for (i in 171:234) {
    assign(paste0("op", i), NULL)
}

## xxx<i> (specials)
op239 <- lengthBlock(uint1, ASCIIchar, blockLabel="string")
op240 <- lengthBlock(uint2, ASCIIchar, blockLabel="string")
op241 <- lengthBlock(int3, ASCIIchar, blockLabel="string")
op242 <- lengthBlock(int4, ASCIIchar, blockLabel="string")

## font_def<i>
font_def <- function(i) {
    k <- switch(i, int1, int2, int3, int4)
    mixedBlock(k=k,
               c=int4,
               s=int4,
               d=int4,
               fontname=markedBlock(mixedBlock(a=int1, l=int1),
                                    function(marker) {
                                        vectorBlock(ASCIIchar,
                                                    blockValue(marker$a) +
                                                    blockValue(marker$l))
                                    },
                                    markerLabel="length",
                                    blockLabel="name"))
}
op243 <- font_def(1)
op244 <- font_def(2)
op245 <- font_def(3)
op246 <- font_def(4)

## pre
op247 <- mixedBlock(i=int1,
                    num=int4,
                    den=int4,
                    mag=int4,
                    comment=lengthBlock(int1, ASCIIchar, blockLabel="string"))

## post
op248 <- mixedBlock(p=int4,
                    num=int4,
                    den=int4,
                    mag=int4,
                    l=int4,
                    u=int4,
                    s=int2,
                    t=int2)

## post_post
op249 <- mixedBlock(q=int4,
                    i=int1,
                    ## There could be up to 7 of these to pad the file
                    ## to a multiple of four bytes, but I will ignore
                    ## that for now
                    sig=vectorBlock(ASCIIchar, 4))

opparams <- function(marker) {
    opcode <- blockValue(marker)
    base::get(paste0("op", opcode))
}

opBlock <- markedBlock(marker=opcode,
                       switch=opparams,
                       markerLabel="opcode",
                       blockLabel="opparams")

opFormat <- memFormat(op=opBlock)

readDVI <- function(f) {
    ## Create a list that is known to be too long
    ops <- vector("list", file.size(f))
    done <- FALSE
    offset <- 0
    length <- 1
    while (!done) {
        op <- readFormat(f, opFormat, offset=offset)
        if (blockValue(op$blocks$op.opcode) == 249) {
            done <- TRUE
        }
        offset <- offset + op$nbytes
        ops[[length]] <- op
        length <- length + 1
    }
    result <- ops[1:(length - 1)]
    class(result) <- "DVI"
    result
}

