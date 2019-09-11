getObjectFunctionArgumentNames <- function(object_o_1, allNames_b_1 = TRUE) {
  on <- getObjectClassKind(object_o_1)
  if (is.na(on)) return(NA)
  cn <- getObjectClassNames(object_o_1)$classname
  ofn <- getObjectFunctionNames(object_o_1, allNames_b_1)
  l <- lapply(ofn, function(f) {
    fa <-  switch(on,
            'environment' = object_o_1[[f]],
            'R6' = object_o_1[[f]],
            'S4' = getMethod(f, signature = cn),
            'S3' = get(paste0(f, '.', cn), mode = 'function'),
            'RC' = eval(parse(text = paste0(cn, '$def@refMethods[["', f, '"]]')))
    )
    x <- retrieveFunctionArgumentNames(fa)
    if (is.null(x)) return(vector('character', 0))
    x
  }) #, simplify = FALSE)
  names(l) <- ofn
  l
}
