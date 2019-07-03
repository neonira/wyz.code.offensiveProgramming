getObjectClassNames <-  function(object_) {
  on <- class(object_)
  list(classname = setdiff(on, c('environment', 'R6'))[1], classnames = on)
}
