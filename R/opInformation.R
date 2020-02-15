opInformation <- function() {

  stratum <- buildIdentityList(c('core', paste0('layer_', 1:3)))
  phasing <- buildIdentityList(c('design', 'build', 'test', 'run', 'maintain', 'evolve', 'transversal'))
  intent <- buildIdentityList(c('parts_building', 'parts_assembly', 'quality_control', 'statistics', 'feedback',
                                'content_generation', 'utilities'))
  category <- buildIdentityList(c('function', 'class', 'data'))
  nature <- buildIdentityList(c('exported', 'internal'))

  buildList <- function(name_s_1, category_s_1, nature_s_1,
                        stratum_s_1, phasing_s_1, intent_s_1) {
    list(name = name_s_1, category = category_s_1, nature = nature_s_1,
         stratum = stratum_s_1, phasing = phasing_s_1, intent = intent_s_1
    )
  }

  bec <- function(name_s_1, stratum_s_1, phasing_s_1, intent_s_1) {
    buildList(name_s_1, category$CLASS, nature$EXPORTED, stratum_s_1, phasing_s_1, intent_s_1)
  }

  bef <- function(name_s_1, stratum_s_1, phasing_s_1, intent_s_1) {
    buildList(name_s_1, category$FUNCTION, nature$EXPORTED, stratum_s_1, phasing_s_1, intent_s_1)
  }

  bif <- function(name_s_1, stratum_s_1, phasing_s_1, intent_s_1) {
    buildList(name_s_1, category$FUNCTION, nature$INTERNAL, stratum_s_1, phasing_s_1, intent_s_1)
  }

  dt <- data.table::rbindlist(list(
    bef('defineEvaluationModes', stratum$LAYER_3, phasing$BUILD, intent$PARTS_BUILDING),
    bef('defineFunctionReturnTypesParameterName', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bef('defineTestCaseDefinitionsParameterName', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bec('EvaluationMode', stratum$CORE, phasing$RUN, intent$PARTS_BUILDING),
    bec('FunctionParameterTypeFactory', stratum$CORE, phasing$RUN, intent$PARTS_BUILDING),
    bec('FunctionParameterName', stratum$CORE, phasing$RUN, intent$PARTS_BUILDING),
    bec('TestCaseDefinition', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bef('verifyClassName', stratum$LAYER_1, phasing$BUILD, intent$QUALITY_CONTROL),
    bef('verifyFunctionName', stratum$LAYER_1, phasing$BUILD, intent$QUALITY_CONTROL),
    bef('verifyObjectNames', stratum$LAYER_1, phasing$RUN, intent$UTILITIES),
    bef('verifyFunctionReturnTypesDefinition', stratum$LAYER_1, phasing$RUN, intent$QUALITY_CONTROL),
    bef('verifyTestCaseDefinitions', stratum$LAYER_1, phasing$RUN, intent$QUALITY_CONTROL),
    bef('retrieveFunctionReturnTypes', stratum$LAYER_1, phasing$RUN, intent$UTILITIES),
    bef('retrieveTestCaseDefinitions', stratum$LAYER_1, phasing$RUN, intent$UTILITIES),
    bef('runFunction', stratum$LAYER_1, phasing$RUN, intent$UTILITIES),
    bef('isAuditable', stratum$CORE, phasing$RUN, intent$UTILITIES),
    bef('opInformation', stratum$LAYER_3, phasing$RUN, intent$FEEDBACK),
    bef('identifyOPInstrumentationLevel', stratum$LAYER_3, phasing$RUN, intent$FEEDBACK),
    bef('exploreObjectNamesVerification', stratum$LAYER_3, phasing$RUN, intent$FEEDBACK),
    bef('findFilesInPackage', stratum$LAYER_3, phasing$RUN, intent$UTILITIES),
    bif('generateStatusSummary', stratum$LAYER_3, phasing$RUN, intent$UTILITIES),
    bif('getClassTypicalFunctionNames', stratum$LAYER_2, phasing$BUILD, intent$PARTS_ASSEMBLY),
    bef('getEllipsisName', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bef('getObjectClassKind', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bef('getObjectClassNames', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bef('getObjectFunctionArgumentNames', stratum$CORE, phasing$RUN, intent$PARTS_BUILDING),
    bef('getObjectFunctionNames', stratum$CORE, phasing$RUN, intent$PARTS_BUILDING),
    bif('hasMainClass', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bif('matchFunctionArguments', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bef('retrieveFactory', stratum$CORE, phasing$RUN, intent$PARTS_ASSEMBLY),
    bef('retrieveFunctionArguments', stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    bef('retrieveFunctionArgumentNames', stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    bef('retrievePackageFunctionNames', stratum$LAYER_3, phasing$RUN, intent$UTILITIES),
    bif('retrieveSupportedObjectInformation', stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    bef('retrieveTestCaseDescriptions', stratum$LAYER_3, phasing$RUN, intent$PARTS_BUILDING),
    bef('runTestCase', stratum$LAYER_3, phasing$RUN, intent$PARTS_BUILDING),
    bef('runTransientFunction', stratum$LAYER_3, phasing$RUN, intent$PARTS_BUILDING),
    bif('verifyName', stratum$LAYER_1, phasing$RUN, intent$PARTS_BUILDING),
    bef('verifyFunctionArguments', stratum$LAYER_3, phasing$BUILD, intent$QUALITY_CONTROL),
    bef('matchFunctionSignature', stratum$LAYER_3, phasing$RUN, intent$PARTS_BUILDING)
  )
  )

  name <- NULL # nse
  dt[order(name)]
}