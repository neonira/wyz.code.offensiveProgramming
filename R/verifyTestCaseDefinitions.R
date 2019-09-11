verifyTestCaseDefinitions <-  function(object_o_1,
                                       requiresFullInstrumentation_b_1 = TRUE,
                                       functionParameterTypeFactory_o_1 = retrieveFactory()) {

  buildReturnValue <- function(validity_b, intent_s, msg_s) {
    list(validity = validity_b,
         check = ifelse(requiresFullInstrumentation_b_1, 'full instrumentation check',
                        'partial instrumentation check'),
         class = getObjectClassNames(object_o_1)$classname,
         intent = intent_s,
         message = msg_s)
  }

  mef <- function(x_s) strBracket(strJoin(x_s))

  verifyFormat <- function(sof_l) {

    brv <- function(validity_b, msg_s) {
      buildReturnValue(validity_b, 'test case definition format', msg_s)
    }

    # verify object name and type
    if (!sof_l$tcd)
      return(brv(FALSE, paste('no parameter', tcdcn, 'definition in class')))

    tc <- sof_l$instrumented_tc
    if (!data.table::is.data.table(tc))
      return(brv(FALSE, paste('apparently instrumented wrongly, expecting data.table, got type',
                              mef(class(tc)))))

    # verify column names
    expected_column_names <- c('function_name', 'standard_evaluation', 'type_checking_enforcement', 'test_case')
    sd <- setdiff(colnames(tc), expected_column_names)
    if (length(sd) != 0)
      return(brv(FALSE, paste('wrong column names in data.table', mef(sd),
                              'expected column names are', mef(expected_column_names))))

    # verify test case type
    typeCheck <- function(x) {
      sapply(x, function(e) {
        getObjectClassNames(e)$classname == 'TestCaseDefinition'
      })
    }
    ch <- typeCheck(tc$test_case)
    if (!all(ch == TRUE))
      return(brv(FALSE, paste('wrong type for test case declaration, entries',
                              mef(which(ch != TRUE)))))

    brv(TRUE, 'test case declaration format verified as valid')
  }

  verifyContent <- function(sof_l) {

    brv <- function(validity_b, msg_s) {
      buildReturnValue(validity_b, 'test case definition content', msg_s)
    }

    tc <- sof_l$instrumented_tc

    # verify function names declared
    declared_functions <- getObjectFunctionNames(object_o_1)
    unknownFun <- setdiff(tc$function_name, declared_functions)
    uninstrumentedFun <- setdiff(declared_functions, tc$function_name)

    if (length(unknownFun) > 0)
      return(brv(FALSE, paste("unknown declared functions", mef(unknownFun))))

    if (requiresFullInstrumentation_b_1 && length(uninstrumentedFun) > 0)
      return(brv(FALSE, paste("uninstrumented functions", mef(uninstrumentedFun))))

    # verify expected status declarations
    allowed_expected_status <- c('correct', 'erroneous', 'failure')
    rv <- tc$standard_evaluation %in% allowed_expected_status
    if (any(rv == FALSE))
      return(brv(FALSE, paste('wrong standard evaluation value',
                              mef(tc$standard_evaluation[which(rv == FALSE)]),
                              'allowed values are', mef(allowed_expected_status))))

    rv <- tc$type_checking_enforcement %in% allowed_expected_status
    if (any(rv == FALSE))
      return(brv(FALSE, paste('wrong type checking enforcement value',
                              mef(tc$type_checking_enforcement[which(rv == FALSE)]),
                              'allowed values are', mef((allowed_expected_status)))))

    brv(TRUE, 'test case declaration content verified as valid')
  }

  tcdcn <- strBracket(defineTestCaseDefinitionsParameterName())

  sof <- retrieveSupportedObjectInformation(object_o_1)
  rv <- verifyFormat(sof)
  if (!rv$validity)
    return(buildReturnValue(FALSE, paste(tcdcn, 'format verification'),
                            paste('failure', rv$intent, rv$message)))

  rv <- verifyContent(sof)
  if (!rv$validity)
    return(buildReturnValue(FALSE, paste(tcdcn, 'content verification'),
                            paste('failure', rv$intent, rv$message)))

  buildReturnValue(TRUE, 'naming and instrumentation format and content seems good', 'success')
}
