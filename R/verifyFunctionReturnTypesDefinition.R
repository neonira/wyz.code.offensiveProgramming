verifyFunctionReturnTypesDefinition <- function(object_o_1,
                                                requiresFullInstrumentation_b_1 = TRUE,
                                                functionParameterTypeFactory_o_1 = retrieveFactory()) {

  buildReturnValue <- function(validity_b, intent_s, msg_s) {
    list(validity = validity_b,
         class = getObjectClassNames(object_o_1)$classname,
         intent = intent_s,
         message = msg_s)
  }

  expected_column_names <- c('function_name', 'return_value')
  frt <- defineFunctionReturnTypesParameterName()
  frtcn <-  strBracket(frt)
  fi <- NA

  mef <- function(x_s) strBracket(strJoin(x_s))

  verifyFormat <- function() {

    brv <- function(validity_b, msg_s) {
      buildReturnValue(validity_b, 'function return types information format', msg_s)
    }

    if (!frt %in% ls(object_o_1))
      return(brv(FALSE, paste('no parameter', frtcn, 'definition in class')))

    fi <<- guardExecution(get(frt, envir = object_o_1))
    if (!data.table::is.data.table(fi))
      return(brv(FALSE, paste('parameter', frtcn, 'wrongly instrumented in class. Must be a data.table')))

    if (length(setdiff(expected_column_names, colnames(fi))) != 0)
      return(brv(FALSE,
                 paste('wrong column name, got', mef(colnames(fi)),
                       'expected were', mef(expected_column_names))))
    brv(TRUE, 'verified correct')
  }

  verifyContent <- function() {

    brv <- function(validity_b, msg_s) {
      buildReturnValue(validity_b, 'function return types information content', msg_s)
    }

    if (length(unique(fi$function_name)) != length(fi$function_name))
      return(brv(FALSE, 'unicity issue with declared function names'))

    ofn <- getObjectFunctionNames(object_o_1)
    sd <- setdiff(fi$function_name, ofn)
    if (length(sd) != 0)
      return(brv(FALSE, paste('unknown function name:', strJoin(strBracket(sd)))))

    sd <- setdiff(ofn, fi$function_name)
    if (requiresFullInstrumentation_b_1 && length(sd) != 0)
      return(brv(FALSE, paste('missing function declarations:', strJoin(strBracket(sd)))))

    rv <- unique(fi$return_value) # need to check that return_value is a known type
    cv <- sapply(rv, function(e) {
      FunctionParameterName(e)$isSemanticName(functionParameterTypeFactory_o_1)
    }, simplify = FALSE)
    if (any(cv == FALSE)) {
      w <- which(cv == FALSE)
      return(brv(FALSE, paste('wrong return value declaration', strBracket(rv[w]))))
    }

    brv(TRUE, 'verified correct')
  }

  rv <- verifyFormat()
  if (!rv$validity)
    return(buildReturnValue(FALSE, paste(frtcn, 'format verification'),
                            paste('failure', rv$intent, rv$message)))

  rv <- verifyContent()
  if (!rv$validity)
    return(buildReturnValue(FALSE, paste(frtcn, 'content verification'),
                            paste('failure', rv$intent, rv$message)))

  buildReturnValue(TRUE, 'naming and instrumentation format and content seems good', 'success')
}