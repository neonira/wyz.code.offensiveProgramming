context("runFunction")

source('pathResolver.R')

source(file.path(computeRootPath(), 'code-samples/fun-defs/good/partial/AdditionFI_Partial.R'))

results <- data.table(
  unnamed_args = c(rep(TRUE, 15)),
  named_args = c(rep(FALSE, 9), rep(TRUE, 6)),
  args_provided = c(rep('same', 3),  rep('more', 3), rep('less', 9)),
  expected_result = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, rep(TRUE, 3), TRUE, FALSE, FALSE),
  execution_result = list(
    runFunction(AdditionFI_Partial(), 'addDouble', list(pi, 1L), EvaluationMode(defineEvaluationModes()[1])),
    runFunction(AdditionFI_Partial(), 'addDouble', list(pi, 1L), EvaluationMode(defineEvaluationModes()[2])),
    runFunction(AdditionFI_Partial(), 'addDouble', list(pi, 1L), EvaluationMode(defineEvaluationModes()[3])),

    # extraneous arg 48
    runFunction(AdditionFI_Partial(), 'addDouble', list(pi, 1L, 48), EvaluationMode(defineEvaluationModes()[1])),
    runFunction(AdditionFI_Partial(), 'addDouble', list(pi, 1L, 48), EvaluationMode(defineEvaluationModes()[2])),
    runFunction(AdditionFI_Partial(), 'addDouble', list(pi, 1L, 48), EvaluationMode(defineEvaluationModes()[3])),

    # missing arg
    runFunction(AdditionFI_Partial(), 'addDouble', list(pi), EvaluationMode(defineEvaluationModes()[1])),
    runFunction(AdditionFI_Partial(), 'addDouble', list(pi), EvaluationMode(defineEvaluationModes()[2])),
    runFunction(AdditionFI_Partial(), 'addDouble', list(pi), EvaluationMode(defineEvaluationModes()[3])),

    # named and unnamed args not in order, not matching default values, good types provided
    runFunction(AdditionFI_Partial(), 'add3segments', list(z = 1:3, 7:9), EvaluationMode(defineEvaluationModes()[1])),
    runFunction(AdditionFI_Partial(), 'add3segments', list(z = 1:3, 7:9), EvaluationMode(defineEvaluationModes()[2])),
    runFunction(AdditionFI_Partial(), 'add3segments', list(z = 1:3, 7:9), EvaluationMode(defineEvaluationModes()[3])),

    ## named and unnamed args not in order, not matching default values, wrong type doubles instead of integers
    runFunction(AdditionFI_Partial(), 'add3segments', list(z = 1:3, c(7, 8, 9)), EvaluationMode(defineEvaluationModes()[1])),
    runFunction(AdditionFI_Partial(), 'add3segments', list(z = 1:3, c(7, 8, 9)), EvaluationMode(defineEvaluationModes()[2])),
    runFunction(AdditionFI_Partial(), 'add3segments', list(z = 1:3, c(7, 8, 9)), EvaluationMode(defineEvaluationModes()[3]))
  )
)

test_that("runFunction", {

  verify_status <- function(i) {
    #wyz.string.ops::catn(i, results[i]$expected_result, results[i]$execution_result[[1]]$status)
    expect_true(results[!!i]$expected_result == results[!!i]$execution_result[[1]]$status)
  }

  sapply(seq_len(nrow(results)), verify_status)
})
