generateStatusSummary <- function(numberErrors_ui, numberTotal_ui, labels_s) {
  mark <- ifelse(!numberErrors_ui, crayon::green('\u2714'), crayon::red('\u2718'))
  paste(crayon::blue(paste0(numberErrors_ui, '/', numberTotal_ui)),
        crayon::blue(labels_s),
        mark,
        collapse = ' | ')
}