context("matchFunctionArguments")

test_that("matchFunctionArguments", {
  expect_true(all(matchFunctionArguments(list(3.4, 4L, pi), c('x_d', 'x_i', 'z_n'))$validity))

  # second argument is double not an int
  expect_false(all(matchFunctionArguments(list(3.4, 4, pi), c('x_d', 'x_i', 'z_n'))$validity))

  # third argument is not a semantic name
  expect_false(all(matchFunctionArguments(list(3.4, 4L, pi), c('x_d', 'x_i', 'z'))$validity))

  # 1 extraneous argument
  expect_false(all(matchFunctionArguments(list(3.4, 4L, pi), c('x_d', 'x_i'))$validity))

  # ellipsis matches all
  expect_true(all(matchFunctionArguments(list(3.4, 4L, pi, 5L, 1:3), c('x_d', 'x_i', '...'))$validity))

  # wrong input value names - duplicated values
  expect_false(all(matchFunctionArguments(list(x = 3.4, y = 4L, x = pi), c('x_d', 'x_i', 'z_d'))$validity))
})
