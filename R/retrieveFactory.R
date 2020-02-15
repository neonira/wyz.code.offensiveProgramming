retrieveFactory <- function() {
  traceFactory <- function(factory_o_1, info_s_1 = 'default') {
    if (isAuditable()) {
      cat(info_s_1,
          'type factory address', data.table::address(factory_o_1),
          '\nsuffixes', strBracket(strJoin(factory_o_1$retrieveKnownSuffixes())),
          "\n")
    }
    factory_o_1
  }

  g <- options('op_type_factory')$op_type_factory
  if (is.environment(g) && is(g, 'FunctionParameterTypeFactory'))
    return(traceFactory(g, 'user defined'))
  traceFactory(FunctionParameterTypeFactory())
}
