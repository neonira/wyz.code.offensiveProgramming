getClassTypicalFunctionNames <- function(object_o_1) {
  on <- getObjectClassKind(object_o_1)
  if (is.na(on)) return(NA)
  switch(on,
         'environment' = vector(mode = 'character', 0),
         'R6' = c('clone'),
         'S3' = vector(mode = 'character', 0),
         'S4' = c('show', 'initialize'),
         'RC' = c("callSuper", "copy", "export", "field", "getClass",
                  "getRefClass", "import", "initFields", "show",  "trace",
                  "untrace", "usingMethods")
  )
}
