FunctionParameterTypeFactory <- function() {
  self <- environment()
  class(self) <- append('FunctionParameterTypeFactory', class(self))

  type_classes <- list(basic = 'basic',
                       math = 'math',
                       data_structure = 'data structure',
                       date = 'date',
                       numeric = 'numeric',
                       language = 'language',
                       feature = 'feature',
                       error = 'error management',
                       user_defined = 'user defined')

  isWarning <- function(o_1l_) 'warning' %in% class(o_1l_)
  isError <- function(o_1l_) 'error' %in% class(o_1l_)

  isPureBoolean <- function(o_1l_) {
    if (!is.logical(o_1l_)) return(FALSE)
    if (length(o_1l_) == 0) return(TRUE)
    all(is.na(o_1l_) == FALSE)
  }

  isPureComplex <- function(o_1l_) {
    if (!is.complex(o_1l_)) return(FALSE)
    if (length(o_1l_) == 0) return(TRUE)
    all(is.na(o_1l_) == FALSE) && all(is.infinite(o_1l_) == FALSE)
  }

  isPureInteger <- function(o_1l_) {
    if (!is.numeric(o_1l_)) return(FALSE)
    if (length(o_1l_) == 0) return(typeof(o_1l_) == 'integer')
    all(is.double(o_1l_) == FALSE)
  }

  isPureMathInteger <- function(o_1l_) {
    if (!isPureInteger(o_1l_)) return(FALSE)
    if (length(o_1l_) == 0) return(TRUE)
    !is.na(o_1l_[1]) && !is.infinite(o_1l_[1])
  }

  isPureReal <- function(o_1l_) {
    if (!is.double(o_1l_)) return(FALSE)
    if (length(o_1l_) == 0) return(TRUE)
    all(is.na(o_1l_[1]) == FALSE) && all(is.infinite(o_1l_) == FALSE)
  }

  isPositiveReal <- function(o_1l_) isPureReal(o_1l_) && all(o_1l_ >= 0.0)

  isNegativeReal <- function(o_1l_) isPureReal(o_1l_) && all(o_1l_ <= 0.0)

  isStrictlyPositiveReal <- function(o_1l_) isPureReal(o_1l_) && all(o_1l_ > 0.0)

  isStrictlyNegativeReal <- function(o_1l_) isPureReal(o_1l_) && all(o_1l_ < 0.0)

  isPositiveInteger <- function(o_1l_) isPureMathInteger(o_1l_) && all(o_1l_ >= 0L)

  isStrictlyPositiveInteger <- function(o_1l_) isPureMathInteger(o_1l_) && all(o_1l_ > 0L)

  isNegativeInteger <- function(o_1l_) isPureMathInteger(o_1l_) && all(o_1l_ <= 0L)

  isStrictlyNegativeInteger <- function(o_1l_) isPureMathInteger(o_1l_) && all(o_1l_ < 0L)

  isString <- function(o_1l_) {
    if (!is.character(o_1l_)) return(FALSE)
    if (length(o_1l_) == 0) return(TRUE)
    all(is.na(o_1l_) == FALSE)
  }

  allowedSuffixes <- list(
    list('a'   , 'array'        , list(is.array)                   , type_classes$data_structure),
    list('b'   , 'boolean'      , list(isPureBoolean)              , type_classes$math),

    list('c'   , 'complex'      , list(is.complex)                 , type_classes$numeric),
    list('cm'  , 'complex-math' , list(isPureComplex)              , type_classes$math),

    list('d'   , 'double'       , list(is.double)                  , type_classes$numeric),
    list('r'   , 'real-math'    , list(isPureReal)                 , type_classes$math),
    list('rm'  , 'real-math alias', list(isPureReal)               , type_classes$math),

    list('ch'  , 'character'    , list(is.character)               , type_classes$basic),
    list('s'   , 'string'       , list(isString)                   , type_classes$basic),

    list('ca'  , 'call'         , list(is.call)                    , type_classes$language),

    list('da'  , 'date'         , list(lubridate::is.Date)         , type_classes$date),
    list('dc'  , 'POSIXct'      , list(lubridate::is.POSIXct)      , type_classes$date),
    list('df'  , 'data.frame'   , list(is.data.frame)              , type_classes$data_structure),
    list('dt'  , 'data.table'   , list(data.table::is.data.table)  , type_classes$data_structure),
    list('dl'  , 'POSIXlt'      , list(lubridate::is.POSIXlt)      , type_classes$date),
    list('dm'  , 'double-math'  , list(isPureReal)                 , type_classes$math),
    list('e'   , 'environment'  , list(is.environment)             , type_classes$basic),
    list('ex'  , 'expression'   , list(is.expression)              , type_classes$language),
    list('er'  , 'error'        , list(isError)                    , type_classes$error),
    list('f'   , 'function'     , list(is.function)                , type_classes$basic),
    list('fa'  , 'factor'       , list(is.factor)                  , type_classes$basic),
    list('i'   , 'integer'      , list(isPureInteger)              , type_classes$numeric),
    list('im'  , 'integer-math' , list(isPureMathInteger)          , type_classes$math),
    list('l'   , 'list'         , list(is.list)                    , type_classes$data_structure),
    list('lo'  , 'logical'      , list(is.logical)                 , type_classes$basic),
    list('m'   , 'matrix'       , list(is.matrix)                  , type_classes$data_structure),
    list('n'   , 'numeric'      , list(is.numeric)                 , type_classes$numeric),
    list('na'  , 'na'           , list(is.na)                      , type_classes$basic),
    list('nm'  , 'name'         , list(is.name)                    , type_classes$language),
    list('o'   , 'object'       , list(is.object)                  , type_classes$basic),

    list('ra'  , 'raw'             , list(is.raw)                  , type_classes$basic),

    list('ui'  , 'unsigned integer', list(isPositiveInteger)      , type_classes$math),
    list('pi'  , 'positive integer', list(isPositiveInteger)      , type_classes$math),
    list('ni'  , 'negative integer', list(isNegativeInteger)      , type_classes$math),
    list('spi' , 'strictly positive integer', list(isStrictlyPositiveInteger), type_classes$math),
    list('sni' , 'strictly negative integer', list(isStrictlyNegativeInteger), type_classes$math),

    list('ur'  , 'unsigned real'   , list(isPositiveReal)         , type_classes$math),
    list('pr'  , 'positive real'   , list(isPositiveReal)         , type_classes$math),
    list('nr'  , 'negative real'   , list(isNegativeReal)         , type_classes$math),
    list('spr' , 'strictly positive real', list(isStrictlyPositiveReal), type_classes$math),
    list('snr' , 'strictly negative real', list(isStrictlyNegativeReal), type_classes$math),

    list('t'  , 'table'            , list(is.table)                 , type_classes$data_structure),
    list('w'  , 'warning'         , list(isWarning)               , type_classes$error)
  )

  suffix <- NULL # data.table NSE issue with Rcmd check
  dt <- data.table::rbindlist(allowedSuffixes)
  data.table::setnames(dt, colnames(dt), c('suffix', 'type', 'verify_function', 'category'))
  stopifnot(all(sapply(dt$verify_function, function(e) is.function(e)) == TRUE))
  dt <- dt[order(suffix)]

  getRowNumber <- function(value_s_1) {
    if (value_s_1 %in% dt$suffix) return(which(dt$suffix == value_s_1))
    if (value_s_1 %in% dt$type) return(which(dt$type == value_s_1))
    NA
  }

  getRecordedTypes <- function() copy(dt[order(suffix)])

  retrieveKnownSuffixes <- function() dt$suffix

  checkSuffix <- function(suffix_s_1) suffix_s_1[1] %in% dt$suffix

  addSuffix <- function(suffix_s_1, type_s_1, typeVerifier_f_1) {
    if (!is.function(typeVerifier_f_1)) return(FALSE)
    i <- identical(retrieveFunctionArguments(isString), retrieveFunctionArguments(typeVerifier_f_1))
    if (!i) return(FALSE)
    s <- gsub('_*([A-Za-z].*)', '\\1', suffix_s_1, perl = TRUE)
    rv <- checkSuffix(s)
    if (!rv) dt <<- data.table::rbindlist(list(dt, list(s, type_s_1, list(typeVerifier_f_1), type_classes$user_defined)))
    !rv
  }

  getType <- function(value_s_1) {
    rn <- getRowNumber(value_s_1[1])
    if (is.na(rn)) return(paste('No suffix or type matches', strBracket(value_s_1[1])))
    dt[rn]$type
  }

  getVerificationFunction <- function(value_s_1) {
    rn <- getRowNumber(value_s_1[1])
    if (is.na(rn)) return(paste('No verification function', strBracket(value_s_1[1])))
    dt[rn]$verify_function[[1]]
  }

  verifyValue <- function(functionParameterName_o, value_) {
    stopifnot(methods::is(functionParameterName_o, 'FunctionParameterName'))

    brv <- function(validity_b, msg_s) {
      list(parameter_name = functionParameterName_o$getFullParameterName(),
           parameter_value = list(value_),
           validity = validity_b,
           message = msg_s)
    }

    checkValue <- function() {
      if (!functionParameterName_o$hasCompliantLength(value_))
        return(brv(FALSE, paste('wrong length, was expecting',
                                strBracket(functionParameterName_o$getLengthSpecification()), ', got',
                                strBracket(length(value_)))))
      rs <- functionParameterName_o$getTypeSuffix()
      if (!rs %in% dt$suffix) return(brv(FALSE, paste0('unknown suffix, [', rs, ']')))
      fn <- dt[suffix == rs]$verify_function[[1]]
      # b <- if (is.list(value_) && !is.object(value_)) all(sapply(value_, fn) == TRUE) else fn(value_)
      b <- fn(value_)
      return(brv(b, paste(ifelse(b, 'good', 'wrong'), 'type in values')))
    }

    if (functionParameterName_o$isPolymorphic()) return(brv(TRUE, 'polymorphic parameter'))
    checkValue()
  }

  getTypeDescription <- function(functionParameterName_o) {

    getAdj <- function(x_s, capitalize_b = FALSE) ifelse(grepl('^[aeiouy]', x_s, perl = TRUE),
                                                         ifelse(capitalize_b, 'An', 'an'),
                                                         ifelse(capitalize_b, 'A', 'a'))

    if (functionParameterName_o$isEllipsis()) return('additional arguments.')
    s <- functionParameterName_o$getTypeSuffix()

    if (checkSuffix(s))  {
      type <- dt[suffix == s]$type
      kind <- if (dt[suffix == s]$category %in% c(type_classes$basic, type_classes$numeric, type_classes$math)) 'values' else 'objects'
    } else {
      type <- if (functionParameterName_o$isPolymorphic()) 'variable type' else 'unknown'
      kind <- 'objects'
    }

    lu <- functionParameterName_o$getLengthSuffix()
    ll <- functionParameterName_o$getLengthModifier()
    constraint <- if (is.na(lu)) 'unconstrained' else {
      if (!is.na(ll))  {
        paste0('length-',
               switch(ll, 'n' = paste('1 or', lu), 'l' = paste(lu, 'or less'), 'm' = paste(lu, 'or more'))
        )
      } else {
        if (lu == 1L) 'single' else paste0('length-', lu)
      }
    }

    single <- !is.na(lu) && is.na(ll) && lu == 1L
    paste0(getAdj(constraint, TRUE), ' ', constraint, ' ',
           if (type == 'list') type else
             paste0(ifelse(single, '', 'vector of '), type, ' ',
                    ifelse(single, substr(kind, 1L, nchar(kind) - 1L), kind)
             )
    )
  }

  self
}

