abort <- function(msg_s_1, ...) {
  stop(paste(msg_s_1, ...))
}
strJoin <- function(text_s, join_s_n = ', ') paste(text_s, sep = '', collapse = join_s_n)

strBracket <- function(text_s_n) {
  paste0('[', text_s_n, ']')
}

destroySpaces <- function(text_s) {
  gsub('[\\s\\b]+', '', text_s, perl = TRUE)
}

removeFilenameExtension <- function(filename_s_1m) {
  b <- stringr::str_count(filename_s_1m, '\\.')
  special_files <- grepl('^\\.+$', filename_s_1m, perl = TRUE)

  rfe <- function(fn_s_1) {
    s <- strsplit(fn_s_1, '.', fixed = TRUE)[[1]]
    paste0(s[-length(s)], collapse = '.')
  }
  vrfe <- Vectorize(rfe)

  ifelse(b != 0 & !special_files,
         vrfe(filename_s_1m),
         filename_s_1m
  )
}

guardExecution <- function(yourExpression_ex, instrumentWarnings_b = TRUE) {
  if (instrumentWarnings_b) {
    tryCatch(yourExpression_ex,
             error = function(e) e,
             warning = function(w) w)
  } else {
    tryCatch(yourExpression_ex,
             error = function(e) e)
  }
}

buildIdentityList <- function(entries_s) {
  d <- toupper(entries_s)
  names(d) <- gsub('[^A-Z0-9_]', '', d, perl = TRUE)
  as.list(d)
}



