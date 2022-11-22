
## Support for op255, which is used by upTeX

## Set text direction
## 0 = vertical
## 1 = horizontal
op255 <- int1

print_op_255 <- function(op) {
    txt <- blockValue(op$blocks$op.opparams)
    str <- paste0("op255        ",
                  "txt=", txt, "\n")
    cat(str)
}

