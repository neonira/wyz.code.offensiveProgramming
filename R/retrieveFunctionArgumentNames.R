retrieveFunctionArgumentNames <- function(fun_f_1) {
  if (is.primitive(fun_f_1)) {
    a <- args(fun_f_1)
    if (is.null(a)) return('')
    formalArgs(a)
  } else formalArgs(fun_f_1)
}