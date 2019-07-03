matchFunctionArguments <- function(realArguments_l,
                                   signatureArguments_s,
                                   functionParameterTypeFactory_o_1 = retrieveFactory()) {

  verifySignature <- function(real_l, theoritical_s, message_s_1) {

    remaining <- theoritical_s
    anm <- sapply(seq_len(length(real_l)), function(k) {
      nm <- names(real_l)[k]
      pn <- if (missing(nm) || is.null(nm) || is.na(nm) ||nchar(nm) == 0) remaining[1] else remaining[pmatch(nm, remaining)]
      if (pn != ellipsis) remaining <<- setdiff(remaining, pn)
      pn
    }, simplify = TRUE)

    if (!use_ellipsis && length(unique(anm)) != length(anm))
      return(list(status = FALSE, message = paste('parameter names mismatch',
                                                  strJoin(anm))))

    rv <- sapply(seq_len(length(real_l)), function(k) {
      #catn('parameter name', anm[k], 'value', strBracket(real_l[[k]]))
      if (anm[k] != ellipsis) {
        functionParameterTypeFactory_o_1$verifyValue(FunctionParameterName(anm[k]), real_l[[k]])
      } else  {
        list(parameter_name = ellipsis,
             parameter_value = list(real_l[[k]]),
             validity = TRUE,
             message = 'ellispsis match all')
      }
    }, simplify = FALSE)
    if (is.na(message_s_1)) rbindlist(rv) else rbindlist(append(rv,
                                                                list(list(parameter_name = '#',
                                                                          parameter_value = NA,
                                                                          validity = FALSE,
                                                                          message = message_s_1))))

  }

  message <- NA_character_
  ellipsis <- '...'
  lsa <- length(signatureArguments_s)
  lra <- length(realArguments_l)
  use_ellipsis <- ellipsis %in% signatureArguments_s
  if (lra > lsa ) {
    if (!use_ellipsis) {
      message <- paste(lra - lsa, 'extraneous arguments found')
    }
  }

  ra <- if (use_ellipsis) realArguments_l else realArguments_l[1:min(lra, lsa)] # force cut
  verifySignature(ra, signatureArguments_s, message)
}
