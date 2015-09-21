#' Reference Class for Data Frame-like Objects
#'
#' Objects from this class behave rather like \code{\link{data.frame}} objects, but are formal reference classes
#' so that computations with them use reference semantics
#' @field data An environment whose objects correspond to the "columns" of a data frame or matrix.
#' @field row.names The names for the "rows", treated as those in a data frame.
dataTable <- setRefClass("dataTable",
  fields = c(
    data = "environment",
    row.names = "data.frameRowLabels"
  )
)

dataTable$methods(
    initialize = function(..., data) {
        'This method takes \\code{data=} as a named argument, but coerces it to be an environment,
instead of requiring that it already be one.'
       if(missing(data))
           callSuper(...)
       else
           callSuper(..., data = as(data, "environment"))
   })

setMethod("[",
    signature(x = "dataTable"),
    function (x, i, j, ..., drop = TRUE)
    {
        if(missing(j))
            j <- objects(x$data)
        if(!is.character(j))
            stop(gettextf("Variables in a dataTable can only be referenced by name, by %s",
                          XR::nameQuote(class(j)[[1]])))
        value <- lapply(j, function(var) { y <- get(var, envir = x$data); y[i]})
        names(value) <- j
        rownames <- x$row.names
        if(is.null(rownames) && length(j)>0) rownames <- seq_along(get(j[[1]], envir = x$data))
        class(value) <- "data.frame"
        attr(value, "row.names") <- rownames[i]
        value
    }
)

setMethod("[<-",
    signature(x = "dataTable"),
    function (x, i, j, ..., value)
    {
        stop("Subset replacement is not allowed for this class:  consider \"data.Edit\"")
    }
)
