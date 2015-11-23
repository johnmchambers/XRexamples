degree <- setClass("degree",
                   contains = "numeric")

setValidity("degree",
   function(object) {
       nbad <-
           length(object) -
              sum(is.na(object) |
                 (object >= -180. & object <= 180.))
      if(nbad)
         gettextf("%d values out of range +/- 180.", as.integer(nbad))
      else
         TRUE
   } )


track <- setClass("track",
    slots = list(lat = "degree", long = "degree",
                 time = "DateTime"))

setMethod("initialize", "track",
    function(.Object, lat, long, ...) {
        .Object@lat <- degree(lat)
        .Object@long <- degree(long)
        callNextMethod(.Object, ...)
    })

setMethod("show", "track",
          function(object) {
              xx <-  rbind(.object@lat, .object@long,
                           format(object@time))
              dimnames(xx) <- list(c("Lat.", "Long.", "Time"),
                                   rep("", ncol(xx)))
              show(xx)
          })
