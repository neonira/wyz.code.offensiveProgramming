retrieveFunctionArguments <- function(fun_f_1) {
  if (is.primitive(fun_f_1)) {
    a <- args(fun_f_1)
    if (is.null(a)) return(a)
    formals(a)
  } else formals(fun_f_1)
}
