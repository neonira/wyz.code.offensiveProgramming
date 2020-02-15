retrieveFunctionReturnTypes <- function(object_o_1) {
  von <- verifyObjectNames(object_o_1)
  if (!von$can_be_typed_checked) return(von)

  v <- verifyFunctionReturnTypesDefinition(object_o_1,
                                           von$is_function_fully_instrumented)
  if (!v$validity) return(v)

  von$sof$instrumented_fn
}