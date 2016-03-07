.countW  <- makeCodeWalker(
    call = function(e, w)
        lapply(e, function(ee) walkCode(ee, w)),
    leaf = function(e, w)
        deparse(e)
    )

codeCountW <- function(expr)
    table(unlist(
        walkCode(expr, .countW),
        recursive = TRUE))
