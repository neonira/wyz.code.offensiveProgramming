retrieveSupportedObjectInformation <- function(object_o_1) {
  if (!is.object(object_o_1)) abort('parameter named object_o_1 is not an object')

  v <- getObjectClassKind(object_o_1)
  supported <- c('environment', 'S3', 'R6', 'S4', 'RC')
  if (!v %in% supported)
    abort(paste('parameter object_o_1 is not of supported type',
                strBracket(v), 'supported types are', strBracket(strJoin(supported))))

  dfrtpn <- defineFunctionReturnTypesParameterName()
  dtcdpn <- defineTestCaseDefinitionsParameterName()

  if (v != 'S4') {
    ll <- ls(object_o_1)
    return(list(frt = dfrtpn %in% ll,
                tcd = dtcdpn %in% ll,
                instrumented_fn = object_o_1[[dfrtpn]],
                instrumented_tc = object_o_1[[dtcdpn]]
    ))
  }

  f <- .hasSlot(object_o_1, dfrtpn)
  t <- .hasSlot(object_o_1, dtcdpn)
  list(
    frt = f,
    tcd = t,
    instrumented_fn = if (f) object_o_1@function_return_types else NA,
    instrumented_tc = if (t) object_o_1@test_case_definitions else NA
  )
}
