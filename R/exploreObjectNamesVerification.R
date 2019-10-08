exploreObjectNamesVerification <- function(object_o_1,
                                           what_s_1 = c('names', 'return type', 'test cases', '*')[1],
                                           functionParameterTypeFactory_o_1 = retrieveFactory()) {

  idx <- switch(tolower(substr(what_s_1, 1, 1)),
                'a' = 2, # arguments
                'p' = 2, # parameters
                'r' = 2, # return
                't' = 3, # return type
                '*' = 4, # all
                1
  )

  r <- verifyObjectNames(object_o_1, functionParameterTypeFactory_o_1)

  if (idx == 1 || idx == 4) {
    w <- which(r$parameter_name_compliance$semantic_naming_check == FALSE)
    l <- c(length(which(r$class_name_compliance == FALSE)),
           length(which(r$function_name_compliance == FALSE)),
           length(w)
    )
    if (l[3] != 0) {
      sapply(w, function(k) {
        cat(crayon::yellow(paste('parameter',
                                 strBracket(r$parameter_name_compliance[k]$parameter_name),
                                 'from function',
                                  strBracket(r$parameter_name_compliance[k]$function_name))), '\n')
      })
    }
    cat(generateStatusSummary(l, c(1, length(r$function_name_compliance),
                                   length(r$parameter_name_compliance$semantic_naming_check)),
                              c('class name', 'function names', 'function parameter names')), '\n')
  }

  if (idx == 2 || idx == 4) {
    if (r$owns_function_return_type_information == FALSE) {
      cat(generateStatusSummary(1, 1, 'no instrumentation of function return type'), '\n')
    } else {
      if (r$is_function_fully_instrumented == FALSE &&
          length(r$missing_functions) == 1 &&
          !is.na(r$missing_functions[1])) {
        cat(crayon::yellow(paste('missing function instrumentation:',
                                 paste(r$missing_functions, collapse = ', '))), '\n')
      }
      l <- ifelse(r$is_function_fully_instrumented == TRUE, 0, length(r$missing_functions))
      cat(generateStatusSummary(l, length(r$function_name_compliance),
                                'functions not instrumented'), '\n')
    }
  }

  if (idx == 3 || idx == 4) {
    if (r$owns_test_case_definitions == FALSE) {
      cat(generateStatusSummary(1, 1, 'no instrumentation of test cases'), '\n')
    } else {
      if (r$is_test_case_fully_instrumented == FALSE &&
          length(r$missing_test_cases) == 1 &&
          !is.na(r$missing_test_cases[1])) {
        cat(crayon::yellow(paste('missing test case instrumentation:',
                                 paste(r$missing_test_cases, collapse = ', '))), '\n')
      }
      l <- ifelse(r$is_test_case_fully_instrumented == TRUE, 0, length(r$missing_test_cases))
      cat(generateStatusSummary(l, length(r$function_name_compliance),
                                'test cases not instrumented'), '\n')
    }
  }

  invisible(r)
}
