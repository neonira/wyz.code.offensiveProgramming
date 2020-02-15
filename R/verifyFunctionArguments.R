verifyFunctionArguments <- function(arguments_l, abort_b_1 = TRUE, verbosity_b_1 = FALSE) {
  l <- length(arguments_l)
  if (l == 0) return(TRUE)
  factory <- retrieveFactory()

  argument_names <- names(arguments_l)
  check <- lapply(seq_len(l), function(k) {
      factory$verifyValue(FunctionParameterName(argument_names[k]), arguments_l[[k]])
  })
  validity <- sapply(check, function(e) e$validity)
  if (verbosity_b_1) {
    print(arguments_l)
    # print(check)
    cat(paste(argument_names, validity, sapply(check, function(e) e$message), '\n'), '\n')
  }
  if (all(validity)) return(TRUE)
  if (abort_b_1) {
    w <- which(validity == FALSE)
    abort('argument mistmatch', strBracket(argument_names[w]), check[[w]]$message)
  }
  FALSE
}
