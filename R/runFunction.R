runFunction <- function(object_o_1, functionName_s_1, arguments_l,
                        evaluationMode_o_1,
                        functionParameterTypeFactory_o_1 = retrieveFactory()) {
  cn <- getObjectClassNames(object_o_1)$classname
  fn <- guardExecution(get(functionName_s_1, envir = object_o_1))
  if (!is.function(fn))
    abort('unable to identify function', strBracket(functionName_s_1),
          'from object of type',
          strBracket(cn))
  if (!is.list(arguments_l))
    abort('arguments_l must be a list, got', strBracket(typeof(arguments_l)))

  buildResult <- function(status_b_1, value_, mode_s_1) list(status = status_b_1, value = value_, mode = mode_s_1)

  rv <- guardExecution(do.call(fn, arguments_l))

  modes <- defineEvaluationModes()
  if (evaluationMode_o_1$is(modes[1])) return(buildResult(TRUE, rv, modes[1]))
  frt <- retrieveFunctionReturnTypes(object_o_1, functionParameterTypeFactory_o_1)
  if (!functionName_s_1 %in% frt$function_name)
    abort('function', strBracket(functionName_s_1),
          'is not instrumented in object of class', strBracket(cn))

  function_name <- NULL # data.table NSE issue with Rcmd check
  b <- functionParameterTypeFactory_o_1$verifyValue(FunctionParameterName(frt[function_name == functionName_s_1]$return_value), rv)
  s <- buildResult(b$validity, rv, modes[2])
  s$function_return_type_check <- rbindlist(list(b))
  if (evaluationMode_o_1$is(modes[2])) return(s)

  fa <- getObjectFunctionArguments(object_o_1)
  mfa <- matchFunctionArguments(arguments_l, fa[[functionName_s_1]], functionParameterTypeFactory_o_1)
  r <- buildResult(all(mfa$validity) && b$validity, rv, modes[3])
  r$parameter_type_checks <- mfa
  r$function_return_type_check <- s$function_return_type_check
  r
}
