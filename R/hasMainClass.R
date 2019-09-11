hasMainClass <- function(object_o_1, classname_s_1) {
  classname_s_1[1] == getObjectClassNames(object_o_1)$classname[1]
}
