destroySpaces <- function(text_s) {
  gsub('[\\s\\b]+', '', text_s, perl = TRUE)
}