
computeRootPath <- function() {
  g <- getwd()
  if (grepl('.Rcheck', g, fixed = TRUE))
    #return(file.path(g, '../../wyz.code.offensiveProgramming'))
    return(system.file(".", package="wyz.code.offensiveProgramming"))
  if (grepl('tests/testthat', g, fixed = TRUE)) return(file.path(g, '../../inst'))
  '.'
}
