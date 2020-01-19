matchFunctionSignature <- function(aFunction_f_1, aFunctionTemplate_f_1 = function(){}) {
  if (!is.function(aFunction_f_1)) return(FALSE)
  identical(
    retrieveFunctionArguments(aFunctionTemplate_f_1),
    retrieveFunctionArguments(aFunction_f_1)
  )
}
