retrieveFunctionReturnTypes <- function(object_o_1,
                                        functionParameterTypeFactory_o_1 = retrieveFactory()) {

  von <- verifyObjectNames(object_o_1, functionParameterTypeFactory_o_1)
  if (!von$can_be_typed_checked) return(von)

  v <- verifyFunctionReturnTypesDefinition(object_o_1,
                                           von$is_function_fully_instrumented,
                                           functionParameterTypeFactory_o_1)
  if (!v$validity) return(v)

  von$sof$instrumented_fn
}