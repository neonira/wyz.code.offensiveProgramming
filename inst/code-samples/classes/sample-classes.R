Accumulator_R6 <- R6::R6Class("Accumulator_R6", list(
  sum = 0,

  add = function(x = 1) {
    self$sum <- self$sum + x
    self$sum
  })
)

EmptyR6 <- R6::R6Class("EmptyR6", list())

FieldR6 <- R6::R6Class("FieldR6", list(alpha = 4.4))

MethodR6 <- R6::R6Class("MethodR6", list(alpha = function() Inf))

BindingR6 <- R6::R6Class("BindingR6", private = list(x = 1000L),
                         active = list(z = function(x_i) {
  if (missing(x_i)) return(private$x)
  private$x <- as.integer(x_i)
}))

Bu_S3 <- function(x_l = list(l = letters, d = 0:9)) {
  value <- x_l
  attr(value, 'class') <- 'Bu_S3'
  value
}

alpha <- function(x, msg_s = '', ...) {
  UseMethod('alpha')
}

alpha.Bu_S3 <- function(x, msg_s = '', ...) {
  paste("in alpha.Bu_S3", class(x), msg_s, ...)
}

EmptyS3 <- function() {
  value <- list()
  attr(value, 'class') <- 'EmptyS3'
  invisible(value)
}

FieldS3 <- function(x_b = list(a = 1L)) {
  value <- x_b
  attr(value, 'class') <- 'FieldS3'
  invisible(value)
}

MethodS3 <- function(x_l = list()) {
  value <- x_l
  attr(value, 'class') <- 'MethodS3'
  invisible(value)
}

value <- function(x, ...) {
  UseMethod('value')
}
value.MethodS3 <- function(x, ...) { x$value }

Unknown <- function() {
  value <- 1L
  attr(value, 'class') <- 'Unknown'
  invisible(value)
}

# alpha(b, "ici", 'là',  'et là-bas')
# [1] "in alpha.Bu_S3 Bu_S3 ici là et là-bas"

print.Bu_S3 <- function(x, ...) {
  print.default(x, ...)
}

getRObjectFromClassKind <- function(classkind_s_1) {
  switch(toupper(classkind_s_1[1]),
         'R6' = Accumulator_R6$new(),
         'RC' = Person_RC(name = 'neonira'),
         'S4' = Person_S4(name = 'neonira'),
         'ENV' = MeltingPot_Env(),
         'S3' = Bu_S3(),
         NULL
  )
}

MeltingPot_Env <- function() {
  self <- environment()
  class(self) <- append('MeltingPot_Env', class(self))

  addNumeric <- function(x_n, y_n) x_n + y_n

  addDouble <- function(x_d, y_d) x_d + y_d

  addInteger <- function(x_i, y_i) x_i + y_i

  addMultiDouble <- function(...) as.double(sum(..., na.rm = TRUE))

  addMultiInteger <- function(x_i, ...) x_i + sum(..., na.rm = TRUE)

  add3length <- function(x_i_3l, y_i_3l = 1:3) x_i_3l + y_i_3l

  divideByZero <- function(x_n) x_n / 0

  generateWarning <- function() 1:3 + 1:7

  generateError <- function() stop('generated error')

  echo <- function(x_s = 'hello world') { x_s }

  blabla <- function(a_s = 'bla', b_s = c('bli', 'blo', 'blu'))
    paste(a_s, b_s)

  self
}

Person_RC <- setRefClass("Person_RC",
                         fields = list(name = "character",
                                       age = "integer"),
                         methods = list(
                           setName = function(aname) {
                             name <<- aname
                           },
                           setAge = function(anAge) {
                             age <<- anAge
                           }
                         )
)

EmptyRC <- setRefClass("EmptyRC",
                       fields = list(),
                       methods = list()
)

FieldRC <- setRefClass("FieldRC",
                       fields = list(x = 'integer'),
                       methods = list()
)

MethodRC <- setRefClass("MethodRC",
                        fields = list(),
                        methods = list(
                          getString = function() {
                            paste(sample(letters, 7, TRUE), collapse = '\n')
                          }
                        )
)

Person_S4 <- setClass("Person_S4",
                      slots = c(
                        name = "character",
                        age = "numeric"
                      )
)

setMethod("show", "Person_S4", function(object) {
  cat(is(object)[[1]], "\n",
      "  Name: ", object@name, "\n",
      "  Age:  ", object@age, "\n",
      sep = ""
  )
})

setGeneric("name", function(o_) standardGeneric("name"))
setMethod("name", "Person_S4", function(o_) o_@name)

#showMethods(class='Person_S4', includeDefs = TRUE)

FieldS4 <- setClass("FieldS4",
                    slots = c(
                      name = "character",
                      age = "numeric"
                    )
)

# no slot implies virtual and non reifiability
VirtualS4 <- setClass("VirtualS4")

MethodS4 <- setClass("MethodS4", slots = c(a = 'numeric'))
setGeneric("info", function(x_) standardGeneric("info"))
setMethod('info', signature(x_ = "MethodS4"), function(x_) 43L)

MyEnv <- function() {
  self <- environment()
  class(self) <- append('MyEnv', class(self))

  f <- function(x_d) x_d
  self
}

EmptyEnv <- function() {
  self <- environment()
  class(self) <- append('EmptyEnv', class(self))
  self
}

FieldEnv <- function() {
  self <- environment()
  x <- 4L
  class(self) <- append('FieldEnv', class(self))
  self
}

MethodEnv <- function() {
  self <- environment()
  fun <- function() 444L
  class(self) <- append('MethodEnv', class(self))
  self
}

Zorg <- function() {
  self <- environment()
  class(self) <- append('Zorg', class(self))

  h <- f <- function(x_d) x_d

  function_name <- NULL # data.table NSE issue with Rcmd check
  function_return_types <- data.table(
    function_name = c('f'),
    return_value = c('x_d')
  )[order(function_name)]
  self
}

# wrong column name in data.table
Zarg <- function() {
  self <- environment()
  class(self) <- append('Zarg', class(self))

  h <- f <- function(x_d) x_d

  function_return_types <- data.table(
    function_names = c('f'),
    return_value = c('x_d')
  )
  self
}

# wrong test case definition - none is an unallowed keyword for standard_evaluation
Zurg <- function() {
  self <- environment()
  class(self) <- append('Zurg', class(self))

  h <- f <- function(x_d) x_d

  test_case_definitions <- data.table(
    function_name = c('f'),
    standard_evaluation = c('none'),
    type_checking_enforcement = c('correct'),
    test_case = list(
      TestCaseDefinition(list(34), 34, 'f nominal')
    )
  )

  self
}

# wrong test case definition - none is an unallowed keyword for type_checking_enforcement
Zirg <- function() {
  self <- environment()
  class(self) <- append('Zirg', class(self))

  h <- f <- function(x_d) x_d

  test_case_definitions <- data.table(
    function_name = c('f'),
    standard_evaluation = c('correct'),
    type_checking_enforcement = c('none'),
    test_case = list(
      TestCaseDefinition(list(34), 34, 'f nominal')
    )
  )

  self
}

Wyx <- function(d_d, y_b_1 = FALSE) {
  self <- environment()
  class(self) <- append('Wyx', class(self))

  h <- f <- function(x_d) x_d + d_d

  self
}

PureR <- function() {
  self <- environment()
  class(self) <- append('PureR', class(self))

  h <- f <- function(x) x
  self
}



