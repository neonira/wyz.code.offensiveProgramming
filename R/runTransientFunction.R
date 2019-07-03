runTransientFunction <- function(function_f_1, arguments_l,
                                 evaluationMode_o_1, function_return_type_s_1,
                                 functionParameterTypeFactory_o_1 = retrieveFactory()) {
  if (!is.function(function_f_1))
    abort('unable to identify function', strBracket(function_f_1))

  if (!is.list(arguments_l))
    abort('arguments_l must be a list, got', strBracket(typeof(arguments_l)))

  buildResult <- function(status_b_1, value_, mode_s_1) list(status = status_b_1, value = value_, mode = mode_s_1)

  rv <- guardExecution(do.call(function_f_1, arguments_l))

  modes <- defineEvaluationModes()
  if (evaluationMode_o_1$is(modes[1])) return(buildResult(TRUE, rv, modes[1]))

  b <- functionParameterTypeFactory_o_1$verifyValue(FunctionParameterName(function_return_type_s_1[1]), rv)
  s <- buildResult(b$validity, rv, modes[2])
  s$function_return_type_check <- rbindlist(list(b))
  if (evaluationMode_o_1$is(modes[2])) return(s)

  mfa <- matchFunctionArguments(arguments_l, formalArgs(function_f_1), functionParameterTypeFactory_o_1)
  r <- buildResult(all(mfa$validity) && b$validity, rv, modes[3])
  r$parameter_type_checks <- mfa
  r$function_return_type_check <- s$function_return_type_check
  r
}
