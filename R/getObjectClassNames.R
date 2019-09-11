getObjectClassNames <-  function(object_o_1) {
  on <- class(object_o_1)
  list(classname = setdiff(on, c('environment', 'R6'))[1], classnames = on)
}
