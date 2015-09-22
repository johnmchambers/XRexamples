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
    initialize = function(..., data = data.frame()) {
        'This method takes \\code{data=} as a named argument, but coerces it to be an environment,
instead of requiring that it already be one.'
       if(missing(data))
           callSuper(...)
       else
           callSuper(..., data = as(data, "environment"))
        if(is.null(row.names)) # likely the data arg was a data.frame
            row.names <<- base::row.names(data)
   })

setMethod("[",
    signature(x = "dataTable"),
    function (x, i, j, ..., drop = TRUE)
    {
        if(missing(j))
            j <- objects(x$data)
        if(!is.character(j))
            stop(gettextf("Variables can only be referenced by name, not by %s",
                          XR::nameQuote(class(j)[[1]])))
        if(missing(i))
            i <- seq_along(x$row.names)
        value <- lapply(j, function(var) { y <- get(var, envir = x$data); y[i]})
        names(value) <- j
        value <- as.data.frame(value)
        row.names(value) <- x$row.names[i]
        value
    }
)

setMethod("[<-",
    signature(x = "dataTable"),
    function (x, i, j, ..., value)
    {
        stop(gettextf(
          "No subset replacement for class %s; use %s methods",
           dQuote(class(x)), '"data.edit"'))
    }
)

dataSave <- setRefClass("dataSave",
   fields = c( saved = "dataTable", time = "DateTime")
   )

dataSave$lock(c("saved", "time"))

dataSave$methods(
    initialize = function(data, ...) {
        if(!missing(data)) {
            saved <<- data$copy()
            time <<- Sys.time()
        }
        callSuper(...)
    })
