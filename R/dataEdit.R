#' Reference Class for  Editing Data Table Objects
#'
#' A subclass of \code{\link{dataTable-class}} objects, in which elements in a column can be edited by supplying
#' new values for a range of rows, and the edits can be undone.
#' @field edits A list that keeps account of the edits performed.
#' @field description A character string with a description of the edit project.
dataEdit <- setRefClass("dataEdit",
      fields = c(
        edits = "list",
        description = "character"),
      contains = "dataTable")

dataEdit$methods(
     edit = function(i, var, value) {
       'Replaces the range i in the variable named var in
        the object by value.
        '
         backup <-
             list(i, var, data[[var]][i])
         data[[var]][i] <<- value
         edits <<- c(edits, list(backup))
         invisible(value)
     },
     undo = function() {
       'Undoes the last edit() operation
        and updates the edits field accordingly.
        '
         if(length(edits)) prev <- edits[[length(edits)]]
         else stop("No more edits to undo")
         edit(prev[[1]], prev[[2]], prev[[3]])
         ## trim the edits list
         length(edits) <<- length(edits) - 2
         invisible(prev)
     }
     )
