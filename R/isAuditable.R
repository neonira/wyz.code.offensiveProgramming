isAuditable <- function() {
  Sys.getenv('OP_AUDIT', names = TRUE) != ''
}
