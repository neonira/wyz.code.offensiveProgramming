retrieveFactory <- function() {
  v <- Sys.getenv('OP_TYPE_FACTORY')
  if (v != '') {
    g <- tryCatch(get(v, envir = parent.frame()), error = function(e) NA)
    if (is.environment(g) && is(g, 'FunctionParameterTypeFactory')) return(g)
  }
  FunctionParameterTypeFactory()
}
