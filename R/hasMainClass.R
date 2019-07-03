hasMainClass <- function(object_, classname_s_1) {
  classname_s_1[1] == getObjectClassNames(object_)$classname[1]
}
