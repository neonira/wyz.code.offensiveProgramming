verifyFunctionReturnTypesDefinition <- function(object_o_1,
                                                requiresFullInstrumentation_b_1 = TRUE) {

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
      buildReturnValue(validity_b, 'function return types information format', msg_s)
    }

    if (!sof_l$frt)
      return(brv(FALSE, paste('no parameter', frtcn, 'definition in class')))

    fn <- sof_l$instrumented_fn
    if (!data.table::is.data.table(fn))
      return(brv(FALSE, paste('parameter', frtcn, 'wrongly instrumented in class. Must be a data.table')))

    expected_column_names <- c('function_name', 'return_value')
    if (length(setdiff(expected_column_names, colnames(fn))) != 0)
      return(brv(FALSE,
                 paste('wrong column name, got', mef(colnames(fn)),
                       'expected were', mef(expected_column_names))))
    brv(TRUE, 'verified correct')
  }

  verifyContent <- function(sof_l) {

    brv <- function(validity_b, msg_s) {
      buildReturnValue(validity_b, 'function return types information content', msg_s)
    }

    fn <- sof_l$instrumented_fn
    if (length(unique(fn$function_name)) != length(fn$function_name))
      return(brv(FALSE, 'unicity issue with declared function names'))

    ofn <- getObjectFunctionNames(object_o_1)
    sd <- setdiff(fn$function_name, ofn)
    if (length(sd) != 0)
      return(brv(FALSE, paste('unknown function name:', strJoin(strBracket(sd)))))

    sd <- setdiff(ofn, fn$function_name)
    if (requiresFullInstrumentation_b_1 && length(sd) != 0)
      return(brv(FALSE, paste('missing function declarations:', strJoin(strBracket(sd)))))

    rv <- unique(fn$return_value) # need to check that return_value is a known type
    cv <- sapply(rv, function(e) {
      FunctionParameterName(e)$isSemanticName()
    }, simplify = FALSE)
    if (any(cv == FALSE)) {
      w <- which(cv == FALSE)
      return(brv(FALSE, paste('wrong return value declaration', strBracket(rv[w]))))
    }

    brv(TRUE, 'verified correct')
  }

  sof <- retrieveSupportedObjectInformation(object_o_1)
  frtcn <-  strBracket(defineFunctionReturnTypesParameterName())

  rv <- verifyFormat(sof)
  if (!rv$validity)
    return(buildReturnValue(FALSE, paste(frtcn, 'format verification'),
                            paste('failure', rv$intent, rv$message)))

  rv <- verifyContent(sof)
  if (!rv$validity)
    return(buildReturnValue(FALSE, paste(frtcn, 'content verification'),
                            paste('failure', rv$intent, rv$message)))

  buildReturnValue(TRUE, 'naming and instrumentation format and content seems good', 'success')
}