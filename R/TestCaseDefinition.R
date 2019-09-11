TestCaseDefinition <- function(params_l, expectedResult_, description_s_1) {
  self <- environment()
  class(self) <- append('TestCaseDefinition', class(self))

  getParams <- function() params_l

  getExpectedResult <- function() expectedResult_

  getDescription <- function() description_s_1

  asList <- function() {
    list(params = params_l, expected_result = expectedResult_, description = description_s_1)
  }

  self
}

print.TestCaseDefinition <- function(x, ...) {
  print(x$asList())
}
