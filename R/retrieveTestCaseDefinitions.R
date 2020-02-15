retrieveTestCaseDefinitions <- function(object_o_1) {

  von <- verifyObjectNames(object_o_1)
  if (!von$owns_test_case_definitions) return(von)

  v <- verifyTestCaseDefinitions(object_o_1,
                                 von$is_test_case_fully_instrumented)

  if (!v$validity) return(v)
  von$sof$instrumented_tc
}
