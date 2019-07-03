verifyName <- function(name_s = 'aSimpleName', strictSyntax_b_1 = TRUE, lowerCaseCamelized_b_1 = TRUE) {
  l <- length(name_s)
  if (l == 0) abort('you must provide a at least one name, got', l)

  if (!is.character(name_s))
    abort('you must provide the name as a character, got,', class(name_s))
  if (any(is.na(name_s)))
    abort('function name can not take the NA value')

  fn <- destroySpaces(name_s)
  if (any(nchar(fn) == 0))
    abort('you must provide the name as a non empty string, got,', strBracket(fn))

  isValid <- function(aname_s) {
    if (strictSyntax_b_1) {
      first_letter <- substr(aname_s, 1, 1)
      if (lowerCaseCamelized_b_1 == TRUE &  !first_letter %in% letters) return(FALSE)
      if (lowerCaseCamelized_b_1 == FALSE & !first_letter %in% LETTERS) return(FALSE)
    }
    pattern <- if (strictSyntax_b_1) '[A-Za-z0-9]+' else '[A-Za-z0-9_.]+'
    nchar(gsub(pattern, '', aname_s, perl = TRUE)) == 0
  }

  ivv <- Vectorize(isValid)
  ivv(fn)
}