setClassUnion("optionalCity", "NULL")
setClass("site", slots = c(latitude = "degree", longitude = "degree"))
setClass("city",
  slots = list(location = "site", name = "character",
     twinCity = "optionalCity"))
setIs("city", "optionalCity")
