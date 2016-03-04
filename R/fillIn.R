fillIn <- function(x) {
    for(j in 1:ncol(x)) {
        nas <- is.na(x[,j])
        x[nas,j] <- mean(x[!nas,j])
    }
    x
}
