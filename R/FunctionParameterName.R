FunctionParameterName <- function(name_s_1) {
  self <- environment()
  class(self) <- append('FunctionParameterName', class(self))

  l <- length(name_s_1)
  if (l != 1)
    abort('you must provide a single function parameter name, got', l)
  if (!is.character(name_s_1))
    abort('you must provide the function parameter name as a character, got,',
          class(name_s_1))

  s <- strsplit(name_s_1, '_')[[1]]
  final_underscore <- endsWith(name_s_1, '_')

  l <- length(s)
  isPreValid <- function() {
    if (l > 1 & l <= 3) return(TRUE)
    if (l == 1 & final_underscore) return(TRUE)
    FALSE
  }

  figure_pattern <- '([1-9][0-9]*)'
  has_type_constraint <- l >= 2 && !grepl(figure_pattern, s[2], perl = TRUE)
  has_length_constraint <- grepl(figure_pattern, s[l], perl = TRUE)
  length_modifier_value <- gsub(figure_pattern, '', s[l], perl = TRUE)
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
      if (is.na(type_suffix)) return(FALSE)
      if (is.na(length_suffix)) return(TRUE)
      if (has_length_constraint) {
        if (length_modifier == 'n' && length_suffix == 1) return(FALSE) # forbidden combination 1n
        if (has_length_modifier) return(has_valid_length_modifier)
        return(TRUE)
      }
      return(TRUE)
    }
    if (has_length_constraint) {
      if (!has_type_constraint) {
        if (!final_underscore) return(FALSE)
      }
      if (is.na(length_suffix)) return(FALSE)
      if (has_length_constraint) {
        if (has_length_modifier) return(has_valid_length_modifier)
        return((final_underscore && !has_type_constraint) || (has_type_constraint && !final_underscore))
      }
      return(TRUE)
    }
    if (final_underscore) return(!has_type_constraint)
    FALSE
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
    if (is.na(length_suffix)) return(NA)
    if (is.na(length_modifier)) return(length_suffix)
    paste0(length_suffix, length_modifier)
  }

  hasCompliantLength <- function(value_) {
    lc <- getLengthSuffix()
    if (!is.na(lc)) {
      if (has_valid_length_modifier) {
        if (length_modifier == 'l') return(length(value_) <= lc)
        if (length_modifier == 'n') return(length(value_) %in% c(1, length_suffix))
        if (length_modifier == 'm') return(length(value_) >= lc)
        return(length(value_) == lc)
      } else {
        FALSE
      }
    }
    TRUE
  }

  toString <- function() {
    v <- if (has_valid_length_modifier) {
      paste0('length modifier ',strBracket(length_modifier))
    } else {
      if (has_length_modifier) {
        if (is.na(length_modifier)) '[no length modifier]'
        else paste0('no valid length modifier[', length_modifier, ']')
      } else '[no length modifier]'
    }
    paste('parameter name', strBracket(parameter_name),
          'type suffix', strBracket(type_suffix),
          'length suffix', strBracket(length_suffix), v)
  }

  self
}

