retrieveFunctionArguments <- function(fun_f_1) {
  if (is.primitive(fun_f_1)) formals(args(fun_f_1)) else formals(fun_f_1)
}

retrieveFunctionArgumentNames <- function(fun_f_1) {
  if (is.primitive(fun_f_1)) formalArgs(args(fun_f_1)) else formalArgs(fun_f_1)
}