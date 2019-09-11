Addition_TCFI_Partial_S4 <- setClass("Addition_TCFI_Partial_S4",
                      slots = c(
                        function_return_types = "data.table",
                        test_case_definitions = "data.table"
                      )
)

setMethod("initialize", "Addition_TCFI_Partial_S4", function(.Object, ...) {
  .Object <- callNextMethod()

  .Object@function_return_types <- data.table(
    function_name = c('addInteger',  'generateError'),
    return_value = c('x_n', 'x_er')
  )

  .Object@test_case_definitions <- data.table(
    function_name = c(rep('addInteger', 9), 'generateError'),
    standard_evaluation = c(rep('correct', 4), 'erroneous', rep('correct', 4), 'failure'),
    type_checking_enforcement = c(rep('correct', 4), rep('failure', 5), 'failure'),
    test_case = list(
      #addInteger - correct
      TestCaseDefinition(list(34L, as.integer(44.5)), 78L, 'sum 2 integers'),
      TestCaseDefinition(list(34L, NA_integer_), NA_integer_, 'sum 1 integer and 1 NA_integer'),
      TestCaseDefinition(list(NA_integer_, NA_integer_), NA_integer_, 'sum 2 NA_integer'),
      TestCaseDefinition(list(as.integer("45.654"), 44L), 89L, 'sum a converted string with one integer'),

      # addInteger - se => erroneous, tci => failure
      TestCaseDefinition(list(34L, 44.5), 78L, 'sum 1 integer and 1 double'),

      # addInteger - se => correct, tci => failure
      TestCaseDefinition(list(34L, Inf), Inf, 'sum 1 integer and 1 Inf'),
      TestCaseDefinition(list(34L, NaN), NaN, 'sum 1 integer and 1 NAN'),
      TestCaseDefinition(list(34L, NA), NA, 'sum 1 integer and 1 NA'),
      TestCaseDefinition(list(c(34L, 35L), 44L), c(78L, 79L), 'sum a vector of 2 integers with 1 integer'),

      # generateError
      TestCaseDefinition(list(), NA, 'generate error')
    )
  )

  #cat("Addition_TCFI_Partial_S4 initialized\n")
  .Object
})

setGeneric(name = "addNumeric", def = function(object_o_1, x_d, y_d) standardGeneric("addNumeric"))

setMethod(f = "addNumeric", signature = "Addition_TCFI_Partial_S4",
          definition = function(object_o_1, x_d, y_d) x_d + y_d
)

setGeneric(name = "addInteger", def = function(object_o_1, x_i, y_i) standardGeneric("addInteger"))

setMethod(f = "addInteger", signature = "Addition_TCFI_Partial_S4",
          definition = function(object_o_1, x_i, y_i) x_i + y_i
)

setGeneric(name = "generateWarning", def = function(object_o_1, x_i, y_i) standardGeneric("generateWarning"))

setMethod(f = "generateWarning", signature = "Addition_TCFI_Partial_S4",
          definition = function(object_o_1) 1:3 + 1:7
)

setGeneric(name = "generateError", def = function(object_o_1) standardGeneric("generateError"))

setMethod(f = "generateError", signature = "Addition_TCFI_Partial_S4",
          definition = function(object_o_1) stop('generated error')
)

# setMethod("addDouble", "Addition_TCFI_Partial_S4", function(object_o_1, x_d, y_d, ...) x_d + y_d)
# setMethod("divideByZero", "Addition_TCFI_Partial_S4", function(object_o_1, x_n, ...) x_n / 0)
