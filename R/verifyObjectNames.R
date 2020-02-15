verifyObjectNames <- function(object_o_1) {

  computeParameterNameCompliance <- function() {

  pn <- getObjectFunctionArgumentNames(object_o_1, FALSE)
    v <- sapply(seq_len(length(pn)), function(k) {
      if (length(pn[[k]]) != 0) {
        sapply(pn[[k]], function(e) {
          fpn <- FunctionParameterName(e)
          list(parameter_name = e,
               name_compliance_check = fpn$isValid(),
               semantic_naming_check = fpn$isSemanticName(),
               function_name = names(pn)[k])
        }, simplify = FALSE)
      } else {
        list(list(parameter_name = NA_character_, name_compliance_check = TRUE,
                  semantic_naming_check = TRUE, function_name = names(pn)[k]))
      }
    }, simplify = FALSE)
    dt <- data.table::rbindlist(unlist(v, recursive = FALSE))
    s <- seq_len(ncol(dt))
    data.table::setcolorder(dt, c(ncol(dt), s[-length(s)]))
    dt
  }

  sof <- retrieveSupportedObjectInformation(object_o_1)

  cn <- getObjectClassNames(object_o_1)$classname
  cct <- verifyClassName(cn, TRUE)
  ccf <- verifyClassName(cn, FALSE)

  ofn <- getObjectFunctionNames(object_o_1)
  if (length(ofn) > 0) {
    fcf <- verifyFunctionName(ofn, FALSE)
    fct <- verifyFunctionName(ofn, TRUE)
    pc <- computeParameterNameCompliance()
    snc <- cct && all(fct) && all(pc$semantic_naming_check)
    slc <- ccf && all(fcf) && all(pc$semantic_naming_check)
  } else {
    fct <- FALSE
    pc <- FALSE
    snc <- FALSE
    slc <- FALSE
  }

  missing_fn <- NA_character_
  ifi <- if (sof$frt) {
    if (is.data.table(sof$instrumented_fn)) {
      defined_fn <- unique(pc$function_name)
      # catn('defined functions', strJoin(defined_fn))
      # catn('instrumented functions', strJoin(instrumented_fn$function_name))
      sd <- setdiff(defined_fn, sof$instrumented_fn$function_name)
      missing_fn <- ifelse(length(sd) == 0, 'none', strJoin(sd))
      length(defined_fn) == nrow(sof$instrumented_fn)
    } else
      FALSE
  } else FALSE

  missing_tc <- NA_character_
  itc <- if (sof$tcd) {
    if (is.data.table(sof$instrumented_tc)) {
      defined_tc <- unique(pc$function_name)
      #catn('defined test cases', strJoin(defined_tc))
      ifn <- unique(sof$instrumented_tc$function_name)
      #catn('instrumented test cases', strJoin(ifn))
      sd <- setdiff(defined_tc, ifn)
      missing_tc <- ifelse(length(sd) == 0, 'none', strJoin(sd))
      length(defined_tc) == length(ifn)
    } else
      FALSE
  } else FALSE

  list(class_name_compliance = cct,
       function_name_compliance = fct,
       parameter_name_compliance = pc,
       classname = cn,
       owns_function_return_type_information = sof$frt,
       owns_test_case_definitions = sof$tcd,
       supports_strict_compliance = snc,
       supports_lazy_compliance = slc,
       can_be_typed_checked = sof$frt && (snc || slc),
       is_function_fully_instrumented = ifi,
       missing_functions = missing_fn,
       is_test_case_fully_instrumented = itc,
       missing_test_cases = missing_tc,
       sof = sof
  )
}


