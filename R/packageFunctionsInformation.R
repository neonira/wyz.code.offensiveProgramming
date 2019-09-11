packageFunctionsInformation <- function() {
  kinds <- c('elaboration', 'verification', 'exploitation', 'information')
  nk <- length(kinds)
  pj <- function(x_i) {
    strJoin(c(kinds, '-')[ifelse(1:nk %in% x_i, x_i, nk + 1)], '|')
  }
  funs <- list(
    list('function' = 'defineEvaluationModes'          ,
         applicability = pj(1:nk), kind = 'information', user = 'developer' ),
    list('function' = 'defineFunctionReturnTypesParameterName',
         applicability = pj(1:nk), kind = 'information', user = 'developer' ),
    list('function' = 'defineTestCaseDefinitionsParameterName',
         applicability = pj(1:nk), kind = 'information', user = 'developer' ),
    list('function' = 'EvaluationMode'                 ,
         applicability = pj(1:nk), kind = 'elaboration', user = 'developer' ),
    list('function' = 'FunctionParameterTypeFactory'   ,
         applicability = pj(1:nk), kind = 'elaboration', user = 'developer' ),
    list('function' = 'FunctionParameterName'          ,
         applicability = pj(1:2) , kind = 'elaboration', user = 'developer' ),
    list('function' = 'TestCaseDefinition'          ,
         applicability = pj(1:2) , kind = 'elaboration', user = 'developer' ),
    list('function' = 'verifyClassName'                ,
         applicability = pj(1:4) , kind = 'verification', user = 'developer' ),
    list('function' = 'verifyFunctionName'             ,
         applicability = pj(1:4) , kind = 'verification', user = 'developer' ),
    list('function' = 'verifyObjectNames'              ,
         applicability = pj(1:4) , kind = 'verification', user = 'developer' ),
    list('function' = 'verifyFunctionReturnTypesDefinition',
         applicability = pj(1:4) , kind = 'verification', user = 'integrator'),
    list('function' = 'verifyTestCaseDefinitions',
         applicability = pj(1:4) , kind = 'verification', user = 'integrator'),
    list('function' = 'retrieveFunctionReturnTypes',
         applicability = pj(1:4) , kind = 'exploitation', user = 'integrator'),
    list('function' = 'retrieveTestCaseDefinitions',
         applicability = pj(1:4) , kind = 'exploitation', user = 'integrator'),
    list('function' = 'runFunction',
         applicability = pj(1:4) , kind = 'exploitation', user = 'end-user'),
    list('function' = 'runTestCases',
         applicability = pj(1:4) , kind = 'exploitation', user = 'end-user'),
    list('function' = 'isAuditable',
         applicability = pj(1:4) , kind = 'exploitation', user = 'end-user'),
    list('function' = 'packageFunctionsInformation'                      ,
         applicability = pj(1:nk), kind = 'information', user = 'anyone' )
  )
  dk <- rbindlist(funs)
  applicability <- NULL # data.table NSE issue with Rcmd check
  dw <- tidyr::separate(dk, applicability, kinds, sep = '\\|')
  sapply(kinds, function(e) dw[[e]] <<- dw[[e]] == e)
  dw
}