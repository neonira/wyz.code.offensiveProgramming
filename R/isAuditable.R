isAuditable <- function() {
  isTRUE(options('op_audit')$op_audit)
}
