getObjectClassKind <-  function(object_o_1) {
  if (!is.object(object_o_1)) return(NA_character_)
  if (isS4(object_o_1)) {
    if (is(object_o_1, 'refClass')) return('RC')
    return('S4')
  }
  on <- class(object_o_1)
  if ('R6' %in% on || 'R6ClassGenerator' %in% on) return('R6')
  if (is.environment(object_o_1)) return('environment')
  if (typeof(object_o_1) == 'list') return('S3')
  'unknown'
}
