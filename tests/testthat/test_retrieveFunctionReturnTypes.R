context("retrieveFunctionReturnTypes")

#Sys.setenv('OP_AUDIT' = 'y')
source('pathResolver.R')
sample_folder <- file.path(computeRootPath(), 'code-samples')

tt <- vector('list', 8)
tt[[1]] <- proc.time()
files <- list.files(sample_folder, pattern = glob2rx('*.R'), recursive = TRUE, full.names = TRUE)
files <- files[grep('sample-classes', files, fixed = TRUE, invert = TRUE)] # not to be considered
#print(files)
.sf <- sapply(files, source, encoding = 'UTF-8', keep.source = TRUE, simplify = FALSE)
names(tt) <- c('start', paste0('source n=', length(files)), 'reify', 'verifyObjectName', 'retrieveFunctionReturnTypes',
                  'verifyType', 'checkResult', 'testthat')
tt[[2]] <- proc.time()

sample_names <- removeFilenameExtension(basename(files))
sample_objects <- sapply(sample_names, function(e) {
  eval(parse(text = paste0(e, ifelse(endsWith(e, 'R6'), '$new', ''), '()')))
})
names(sample_objects) <- files
#print(sample_objects)
tt[[3]] <- proc.time()

fptf <- FunctionParameterTypeFactory()
name_compliance <- lapply(sample_objects, verifyObjectNames, fptf)

l <- length(name_compliance[[1]])
dcond <- rbindlist(lapply(name_compliance, function(e) e[4:(l - 1)]))
#print(dcond)
tt[[4]] <- proc.time()

tcd <- lapply(sample_objects, retrieveFunctionReturnTypes, fptf)
tt[[5]] <- proc.time()

tcd_b <- unlist(lapply(tcd, function(e) is.data.table(e)))
tt[[6]] <- proc.time()

expectedResult <- function(files_s) {
  data.table(file = files_s,
             expected_result =
               ifelse(grepl('frt-defs|both-defs', files_s, perl = TRUE),
                      grepl('good', files_s, perl = TRUE),
                      FALSE)
  )
}

er <- expectedResult(files)

balance <- copy(er)
balance[, `:=`(result = tcd_b)]
balance[, `:=`(status = ifelse(result == expected_result, 'success', 'failure'), fn = basename(file))]
if (nrow(balance[status == 'failure']) > 0) print(balance[status == 'failure'])
tt[[7]] <- proc.time()

test_that("retrieveFunctionReturnTypes", {
  mtf <- function(k) {
    expect_equal(balance$status[!!k], 'success')
  }

  lapply(seq_len(nrow(balance)), mtf)
})

tt[[8]] <- proc.time()

if (isAuditable()) {
  sapply(seq_len(length(tt) - 1), function(k) cat(names(tt)[k], (tt[[k + 1]] - tt[[k]])[3], '\n'))
}