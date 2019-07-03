getObjectFunctionNames <- function(object_, allNames_b = FALSE) {

  filterNames <- function(x) {
    if (allNames_b) return(x)
    setdiff(x, getRClassTypicalFunctionNames(object_))
  }

  filterOutFunctions <- function(names_s, obj_) {
    v <- sapply(names_s, function(e) typeof(obj_[[e]]) == 'closure')
    names(v[which(v == TRUE)])
  }

  on <- getRObjectClassKind(object_)
  if (is.na(on)) return(NA)
  cn <- getObjectClassNames(object_)$classname
  if (on %in% c('environment', 'R6')) {
    return(filterNames(filterOutFunctions(ls(envir = object_, all.names = TRUE), object_)))
  }
  if (on == 'RC') {
    x <- get(cn)
    return(filterNames(filterOutFunctions(ls(envir = x$def@refMethods, all.names = TRUE), x$def@refMethods)))
  }
  if (on %in% c('S3', 'S4')) {
    d <- methods(class = cn)
    return(filterNames(attributes(d)$info$generic))
  }

  NA
}
