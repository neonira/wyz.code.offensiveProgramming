getRObjectClassKind <-  function(object_) {
  if (!is.object(object_)) return(NA)
  if (isS4(object_)) {
    if (is(object_, 'refClass')) return('RC')
    return('S4')
  }
  on <- class(object_)
  if ('R6' %in% on) return('R6')
  if (is.environment(object_)) return('environment')
  if (typeof(object_) == 'list') return('S3')
  'unknown'
}
