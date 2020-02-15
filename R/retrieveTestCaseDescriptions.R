retrieveTestCaseDescriptions <- function(object_o_1) {
  tcd <- retrieveTestCaseDefinitions(object_o_1)
  if (!is.data.table(tcd)) return('provided object owns no test case definitions')
  data.table(function_name = tcd$function_name,
             description = sapply(1:nrow(tcd),
                                  function(e) tcd[e]$test_case[[1]]$getDescription()))
}
