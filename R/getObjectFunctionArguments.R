getObjectFunctionArguments <- function(object_) {
  on <- getRObjectClassKind(object_)
  if (is.na(on)) return(NA)
  cn <- getObjectClassNames(object_)$classname
  ofn <- getObjectFunctionNames(object_, TRUE)
  l <- lapply(ofn, function(f) {
    fa <-  switch(on,
            'environment' = object_[[f]],
            'R6' = object_[[f]],
            'S4' = getMethod(f, signature = cn),
            'S3' = get(paste0(f, '.', cn), mode = 'function'),
            'RC' = eval(parse(text = paste0(cn, '$def@refMethods[["', f, '"]]')))
    )
    x <- formalArgs(fa)
    if (is.null(x)) return(vector('character', 0))
    x
  }) #, simplify = FALSE)
  names(l) <- ofn
  l
}
