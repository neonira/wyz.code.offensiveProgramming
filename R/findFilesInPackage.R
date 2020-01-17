findFilesInPackage <- function(filenames_s, packageName_s_1) {
  sapply(filenames_s, function(e) {
    file.path(list.files(system.file(package = packageName_s_1),
                         e, recursive = TRUE, full.names = TRUE))
  })
}
