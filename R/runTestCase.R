runTestCase <- function(object_o_1, testCaseIndexes_i, evaluationMode_o_1 = EvaluationMode()) {
  tcd <- retrieveTestCaseDefinitions(object_o_1)
  l <- sapply(seq_len(nrow(tcd[testCaseIndexes_i])), function(k) {
    rf <- runFunction(object_o_1, tcd[k]$function_name[[1]], tcd[k]$test_case[[1]]$getParams(), evaluationMode_o_1)
    rf$index <- testCaseIndexes_i[k]
    rf
  }, simplify = FALSE)
  names(l) <- tcd[testCaseIndexes_i]$function_name
  l

  pnames <- c('value', 'parameter_type_checks', 'function_return_type_check')
  list(raw = l, synthesis = rbindlist(lapply(l, function(e) e[which(!names(e) %in% pnames)])))
}
