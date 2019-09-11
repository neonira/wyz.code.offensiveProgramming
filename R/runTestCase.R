runTestCase <- function(object_o_1, testCaseIndexes_i, evaluationMode_o_1 = EvaluationMode()) {

  determineFailureOrigin <- function(values_, frt_l_1, pc_l_1) {
    v <- c('code execution', 'function return type check', 'parameter check')
    r <- v[c('error' %in% class(values_), isTRUE(frt_l_1), isTRUE(pc_l_1))]
    if (length(r) == 0) NA_character_ else strJoin(r)
  }

  compareValue <- function(a_1_, b_1_) {
    len <- c(length(a_1_), length(b_1_))
    if (len[1] != len[2]) return(FALSE)
    if (len[1] == 0) return(typeof(a_1_) == typeof(b_1_))

    rv <- sapply(seq_len(len[1]), function(k) {
      if (length(a_1_[k]) != 1) return(compareValue(a_1_[k], b_1_[k]))
      if (is.nan(a_1_[k])) return(is.nan(b_1_[k]))
      if (is.na(a_1_[k])) return(is.na(b_1_[k]))
      a_1_[k] == b_1_[k]
    }, simplify = FALSE)

    all(unlist(rv))
  }


  tcd <- retrieveTestCaseDefinitions(object_o_1)
  ft <- tcd[testCaseIndexes_i]
  ems <- ifelse(evaluationMode_o_1$getEvaluationMode() == defineEvaluationModes()[1],
                'standard_evaluation', 'type_checking_enforcement')
  l <- sapply(seq_len(nrow(ft)), function(k) {
    rf <- runFunction(object_o_1, ft[k]$function_name[[1]],
                      ft[k]$test_case[[1]]$getParams(),
                      evaluationMode_o_1)
    rf$index <- testCaseIndexes_i[k]
    vc <- compareValue(rf$value, ft[k]$test_case[[1]]$getExpectedResult())
    rf$value_check <- vc
    rf$expected_evaluation <- unlist(ft[k, ems, with = FALSE], use.names = FALSE)
    fo <- determineFailureOrigin(rf$value, !rf$function_return_check,
                                 !rf$parameter_check)
    rf$execution_evaluation <- ifelse(is.na(fo),
                                        ifelse(vc, 'correct', 'erroneous'),
                                        'failure')
    rf$failure_origin <- fo
    rf
  }, simplify = FALSE)
  names(l) <- ft$function_name

  pnames <- c('value', 'parameter_type_checks', 'function_return_type_check')
  syn <- rbindlist(lapply(l, function(e) e[which(!names(e) %in% pnames)]))
  cn <- colnames(syn)
  setcolorder(syn, c(cn[1:2], cn[5:6], cn[3:4], cn[7:9]))
  list(raw = l, synthesis = syn)
}
