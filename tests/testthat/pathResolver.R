computeRootPath <- function() {
  g <- getwd()

  # for Rcheck
  if (grepl('.Rcheck', g, fixed = TRUE))
    return(system.file(package = "wyz.code.offensiveProgramming"))

  # for covr - must be done prior test for testthat
  if (grepl('wyz.code.offensiveProgramming-tests/testthat', g, fixed = TRUE))
    return(system.file(package = "wyz.code.offensiveProgramming"))

  # for testthat
  if (grepl('tests/testthat', g, fixed = TRUE))
    return(normalizePath(file.path(g, '../../inst')))

  '.'
}
