EvaluationMode <- function(value_s_1 = defineEvaluationModes()[2]) {
  self <- environment()
  class(self) <- append('EvaluationMode', class(self))

  if (!value_s_1 %in% defineEvaluationModes())
    abort('unauthorized value', strBracket(value_s_1))

  is <- function(aValue_s_1) {
    aValue_s_1 == value_s_1
  }

  getEvaluationMode <- function() value_s_1

  toString <- function() {
    paste('evaluation mode', strBracket(value_s_1))
  }

  self
}

print.EvaluationMode <- function(x, ...) cat(x$toString(), ..., '\n')

