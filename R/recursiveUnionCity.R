setClassUnion("optionalCity", "NULL")
setClass("city",
  slots = list(location = "site", name = "character",
     twinCity = "optionalCity"))
setIs("city", "optionalCity")
