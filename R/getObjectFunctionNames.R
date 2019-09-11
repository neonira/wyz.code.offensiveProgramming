getObjectFunctionNames <- function(object_o_1, allNames_b_1 = FALSE) {

  filterNames <- function(x) {
    if (allNames_b_1) return(x)
    setdiff(x, getClassTypicalFunctionNames(object_o_1))
  }

  filterOutFunctions <- function(names_s, obj_1_) {
    v <- sapply(names_s, function(e) is.function(obj_1_[[e]]))
    names(v[which(v == TRUE)])
  }

  on <- getObjectClassKind(object_o_1)
  if (is.na(on)) return(NA)
  cn <- getObjectClassNames(object_o_1)$classname
  if (on %in% c('environment', 'R6')) {
    return(filterNames(filterOutFunctions(ls(envir = object_o_1, all.names = TRUE), object_o_1)))
  }
  if (on == 'RC') {
    x <- get(cn)
    return(filterNames(filterOutFunctions(ls(envir = x$def@refMethods, all.names = TRUE),
                                          x$def@refMethods)))
  }
  if (on %in% c('S3', 'S4')) {
    d <- suppressWarnings(methods(class = cn))
    return(filterNames(attributes(d)$info$generic))
  }

  NA
}
