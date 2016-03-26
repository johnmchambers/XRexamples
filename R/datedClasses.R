#' Classes Adding DateTime slot/field to Basic Classes
#'
#' These classes all add a simple \code{date} slot or field to data
#' of a basic type.  As described in Chapter 10, class \code{"datedTable"}
#' is a reference class, with a field for the environment data.
#'
#' All the classes have corresponding generating functions of the same name
#' that will take \code{date=} arguments for that slot/field, unnamed arguments
#' for the data part, except for  \code{datedTable} which needs \code{table=}
#' for the environment to use.
#'
#' @slot date The date information, using class \code{"DateTime"} to include
#' any of the classes for representing such information.
datedText <- setClass("datedText", contains = "character",
                      slots = list(date = "DateTime"))

#' @rdname datedText-class
datedVector <- setClass("datedVector", contains = "vector",
                        slots = list(date = "DateTime"))

#' @rdname datedText-class
datedList <- setClass("datedList", contains = "namedList",
         slots = list(date = "DateTime"))

#' @rdname datedText-class
datedTable <- setRefClass("datedTable",
      fields = list(table = "environment", date ="DateTime"),
      methods = list(
        entry = function(what, value) {
            table[[what]] <<- value
            date <<- Sys.time()
        })
    )
