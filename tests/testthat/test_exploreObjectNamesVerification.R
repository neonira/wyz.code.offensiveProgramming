context("exploreObjectNamesVerification")

source('pathResolver.R')
source(file.path(computeRootPath(), 'code-samples', 'classes', 'sample-classes.R'))

source(file.path(computeRootPath(), 'code-samples/no-defs/Addition.R'))
source(file.path(computeRootPath(), 'code-samples/frt-defs/good/partial/AdditionFI_Partial.R'))
source(file.path(computeRootPath(), 'code-samples/tcd-defs/good/partial/AdditionTC_Partial.R'))
source(file.path(computeRootPath(), 'code-samples/both-defs/good/partial/Addition_TCFI_Partial_S4.R'))
source(file.path(computeRootPath(), 'code-samples/both-defs/good/full/AdditionTCFIG1.R'))

obj <- list(Addition(), AdditionFI_Partial(), AdditionTC_Partial(),
            Addition_TCFI_Partial_S4(), AdditionTCFIG1())

test_that("exploreObjectNamesVerification", {
  sapply(seq_len(length(obj)), function(k) {
    expect_output(exploreObjectNamesVerification(obj[[k]], 'n'))
    expect_output(exploreObjectNamesVerification(obj[[k]], 'p'))
    expect_output(exploreObjectNamesVerification(obj[[k]], 't'))
  })
})
