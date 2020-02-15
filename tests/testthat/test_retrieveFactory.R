context("retrieveFactory")

resetOptions <- function() {
  options('op_type_factory' = NULL)
}

setOptions <- function(rObject_) {
  options('op_type_factory' = rObject_)
}

getOptions <- function() { options('op_type_factory') }

resetOptions()
r1 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz'])

my_factory <- retrieveFactory()
my_factory$addSuffix('xyz', 'xyz', function(o_1l_) is(o_1l_, 'xyz'))

# no environment variable associated
setOptions('')
r2 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz'])
#cat('r2', r2, '\n')

# set incorrect R variable - passed name not object
setOptions('my_factory')
r3 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz'])
#cat('r3', r3, '\n')

# set correct R variable name
setOptions(my_factory)
r4 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz']) + 0
#cat('r4', r4, '\n')

# R variable exists but no environement variable set
resetOptions()
r5 <- nrow(retrieveFactory()$getRecordedTypes()[suffix == 'xyz'])
#cat('r5', r5, '\n')

#
rm('my_factory')
ls()
setOptions('my_factory')
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


