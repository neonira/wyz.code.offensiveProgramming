identifyOPInstrumentationLevel <- function(object_o_1 = NULL,
                                           methodName_s_1 = NA_character_) {

  if (is.null(object_o_1) || !is.object(object_o_1)) {
    b1 <- if (is.na(methodName_s_1)) FALSE else {
      args <- retrieveFunctionArgumentNames(methodName_s_1)
      if (is.null(args)) FALSE else {
        all(sapply(args, function(e) {
          FunctionParameterName(e)$isValid()
        }))
      }
    }
    list(offensive_programming = FALSE,
         full_instrumentation = FALSE,
         semantic_naming = b1,
         function_return_type = FALSE,
         test_case_definition = FALSE)
  } else {
    v <- verifyObjectNames(object_o_1)

    b1 <- if (is.logical(v$parameter_name_compliance)) {
      v$parameter_name_compliance
      } else {
        all(v$parameter_name_compliance$semantic_naming_check)
      }
    b2 <- v$owns_function_return_type_information
    b3 <- v$owns_test_case_definitions
    list(offensive_programming = b1 && (b2 || b3), # manages partial instrumentation
         full_instrumentation = b1 && b2 && b3,
         semantic_naming = b1,
         function_return_type = b2,
         test_case_definition = b3)
  }
}
