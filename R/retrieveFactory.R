retrieveFactory <- function(functionParameterTypeFactory_o_1 = NULL) {
  epn <- 'typeFactory'
  b <- is.null(functionParameterTypeFactory_o_1) || is.na(functionParameterTypeFactory_o_1)
  reassign <- FALSE
  o <- if (!b) {
    reassign <- TRUE
    functionParameterTypeFactory_o_1
  } else {
    l <- guardExecution(get(epn, envir = .GlobalEnv))

    if ('simpleError' %in% class(l)) {
      reassign <- TRUE
      FunctionParameterTypeFactory()
    } else {
      if (!hasMainClass(l,'FunctionParameterTypeFactory'))
        abort('provided parameter is not of the expected class. Must be',
              strBracket('FunctionParameterTypeFactory'), 'got',
              strBracket(strJoin(class(l))))

      l
    }
  }

  if (reassign) assign(epn, o, envir = .GlobalEnv)
  o
}