codeCountA <- function(expr) {
    ee <- callAsList(expr)
    table(rapply(ee,
        function(x) deparse(x)))
}

