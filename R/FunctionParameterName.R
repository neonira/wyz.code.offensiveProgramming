FunctionParameterName <- function(name_s_1) {
  self <- environment()
  class(self) <- append('FunctionParameterName', class(self))

  l <- length(name_s_1)
  if (l != 1)
    abort('you must provide a single function parameter name, got', l)
  if (!is.character(name_s_1))
    abort('you must provide the function parameter name as a character, got,',
          class(name_s_1))

  final_underscore <- endsWith(name_s_1, '_')
  s <- strsplit(name_s_1, '_')[[1]]

  l <- length(s)
  isPreValid <- function() {
    if (l > 1 && l <= 3) return(TRUE)
    if (l == 1 && final_underscore) return(TRUE)
    FALSE
  }

  figure_pattern <- '([1-9][0-9]*)'
  has_type_constraint <- l >= 2 && !grepl(figure_pattern, s[2], perl = TRUE)
  has_length_constraint <- if (l >= 2) grepl(figure_pattern, s[l], perl = TRUE) else FALSE
  length_modifier_value <- if (l >= 2) gsub(figure_pattern, '', s[l], perl = TRUE) else FALSE
  has_length_modifier <- nchar(length_modifier_value) > 0
  has_valid_length_modifier <- ifelse(has_length_constraint, grepl('(l|m|n)$', s[l], perl = TRUE), FALSE)
  length_modifier <- if (has_length_constraint) substring(s[l], nchar(s[l])) else NA

  if (isPreValid()) {
    parameter_name <- s[1]

    type_suffix <- if (has_type_constraint) {
      s[2]
    } else NA

    length_suffix <- if (has_length_constraint) {
      tryCatch({
        v <- regmatches(s[l], regexpr(figure_pattern, s[l]))
        k <- as.integer(v)
        if (k == 0) NA else k
      }, error = function(e) NA)
    } else NA

  } else {
    parameter_name <- name_s_1
    type_suffix <- length_suffix <- NA
  }

  isEllipsis <- function() parameter_name == '...'

  isValid <- function() {
    if (isEllipsis()) return(TRUE)
    if (!isPreValid()) return(FALSE)
    if (!substr(parameter_name, 1, 1) %in% letters) return(FALSE)
    if (has_type_constraint) {
      if (final_underscore) return(FALSE)
      if (is.na(length_suffix)) return(TRUE)
      if (has_length_constraint) {
        if (length_modifier == 'n' && length_suffix == 1) return(FALSE) # forbidden combination 1n
        if (has_length_modifier) return(has_valid_length_modifier)
        return(TRUE)
      }
      # no fall trhu required
    }
    if (has_length_constraint) {
      if (!has_type_constraint && !final_underscore) return(FALSE)
      if (has_length_modifier) return(has_valid_length_modifier)
      return((final_underscore && !has_type_constraint) ||
               (has_type_constraint && !final_underscore))
    }
    if (final_underscore) return(!has_type_constraint)
    # no fall trhu required
  }

  isSemanticName <- function(functionParameterTypeFactory_o = retrieveFactory()) {
    if (!isValid()) return(FALSE)
    if (isEllipsis()) return(TRUE)
    if (isPolymorphic()) return(TRUE)
    functionParameterTypeFactory_o$checkSuffix(getTypeSuffix())
  }

  getFullParameterName <- function() name_s_1

  getParameterName <- function() parameter_name

  getTypeSuffix <- function() type_suffix

  isPolymorphic <- function() final_underscore

  getLengthSuffix <- function() length_suffix

  getLengthModifier <- function() {
    if (has_valid_length_modifier) return(length_modifier)
    NA
  }

  deduceParameterLabel <- function() {
    tolower(gsub('([A-Z])', ' \\1', parameter_name, perl = TRUE))
  }

  getLengthSpecification <- function() {
    if (is.na(length_suffix)) return(NA_character_)
    if (is.na(getLengthModifier())) return(as.character(length_suffix))
    paste0(length_suffix, length_modifier)
  }

  hasCompliantLength <- function(value_) {
    lc <- getLengthSuffix()
    if (is.na(lc)) return(TRUE)
    l <- length(value_)
    if (!has_valid_length_modifier) return(l == lc)
    if (length_modifier == 'l') return(l <= lc)
    if (length_modifier == 'n') return(l == 1 || l == length_suffix)
    if (length_modifier == 'm') return(l >= lc)
    # no fallthru needed
  }

  toString <- function() {
    paste('parameter name', strBracket(parameter_name),
          'type suffix', strBracket(type_suffix),
          'length suffix', strBracket(length_suffix),
          'length modifier', strBracket(ifelse(has_valid_length_modifier,
                                               length_modifier,
                                               'no length modifier')))
  }

  self
}

print.FunctionParameterName <- function(x, ...) {
  cat(x$toString(), '\n')
}

