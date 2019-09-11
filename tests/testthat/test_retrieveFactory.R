context("retrieveFactory")

resetEnv <- function() {
  if (Sys.getenv('OP_TYPE_FACTORY') != '') Sys.setenv('OP_TYPE_FACTORY' = '')
}

setEnv <- function(rVariableName_s_1) {
  Sys.setenv('OP_TYPE_FACTORY' = rVariableName_s_1)
}

getEnv <- function() { Sys.getenv('OP_TYPE_FACTORY') }

resetEnv()
r1 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz'])
#cat('r1', r1, '\n')

my_factory <- retrieveFactory()
my_factory$addSuffix('xyz', 'xyz', function(e) is(e, 'xyz'))

# no environment variable associated
setEnv('')
r2 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz'])
#cat('r2', r2, '\n')

# set incorrect R variable name
setEnv('my_factor')
r3 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz'])
#cat('r3', r3, '\n')

# set correct R variable name
setEnv('my_factory')
r4 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz']) + 0
#cat('r4', r4, '\n')

# R variable exists but no environement variable set
resetEnv()
r5 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz'])
#cat('r5', r5, '\n')

#
rm('my_factory')
ls()
setEnv('my_factory')
r6 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz'])
#cat('r6', r6, '\n')

test_that("retrieveFactory", {
  expect_equal(r1, 0)
  expect_equal(r2, 0)
  expect_equal(r3, 0)
  expect_equal(r4, 1)
  expect_equal(r5, 0)
  expect_equal(r6, 0)
})


