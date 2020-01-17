retrievePackageFunctionNames <- function(packageName_s_1, libraryPath_s_1 = .libPaths()[1]) {
  if (!packageName_s_1 %in% installed.packages()[, 'Package'])
    abort('package', strBracket(packageName_s_1), 'is not installed')

  sn <- packageName_s_1
  if (!sn %in% search()) {
    tt <- paste0('package:', packageName_s_1)
    if (!tt %in% search()) library(packageName_s_1, character.only = TRUE)
    sn <- tt
  }
  if (sn %in% search()) {
    l <- ls(sn, all.names = TRUE)
    return(l[sapply(l, function(e) is.function(get(e)))])
  }
  abort('package', strBracket(packageName_s_1), 'not found in search path')
}
