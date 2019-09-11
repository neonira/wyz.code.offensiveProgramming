context("verifyObjectNames")

source('pathResolver.R')
source(file.path(computeRootPath(), 'code-samples', 'classes', 'sample-classes.R'))

source(file.path(computeRootPath(), 'code-samples/both-defs/good/partial/Addition_TCFI_Partial_S4.R'))
source(file.path(computeRootPath(), 'code-samples/both-defs/good/partial/Addition_TCFI_Partial_S3.R'))

test_that("verifyObjectNames", {
   expect_error(verifyObjectNames(new.env()))
   expect_false(verifyObjectNames(Bu_S3())$owns_function_return_type_information)
   expect_false(verifyObjectNames(Bu_S3())$owns_test_case_definitions)

   expect_false(verifyObjectNames(new('Person_S4', name = 'neonira'))$owns_function_return_type_information)
   expect_false(verifyObjectNames(new('Person_S4', name = 'neonira'))$owns_test_case_definitions)

   expect_false(verifyObjectNames(new('Person_RC', name = 'neonira'))$owns_function_return_type_information)
   expect_false(verifyObjectNames(new('Person_RC', name = 'neonira'))$owns_test_case_definitions)

   expect_false(verifyObjectNames(Accumulator_R6$new())$owns_function_return_type_information)
   expect_false(verifyObjectNames(Accumulator_R6$new())$owns_test_case_definitions)

   expect_false(verifyObjectNames(MyEnv())$owns_function_return_type_information)
   expect_false(verifyObjectNames(MyEnv())$owns_test_case_definitions)

   # expect_true(verifyObjectNames(Addition_TCFI_Partial_S3())$owns_function_return_type_information)
   # expect_true(verifyObjectNames(Addition_TCFI_Partial_S3())$owns_test_case_definitions)

   expect_true(verifyObjectNames(new('Addition_TCFI_Partial_S4'))$owns_function_return_type_information)
   expect_true(verifyObjectNames(new('Addition_TCFI_Partial_S4'))$owns_test_case_definitions)
})
