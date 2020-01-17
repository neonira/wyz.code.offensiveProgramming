citefun <- function(x_s) paste0('<cite class="it">', x_s, '</cite>')
citeop <- function(x_s) paste0('<cite class="op">', x_s, '</cite>')
citearg <- function(x_s) paste0('<cite class="os">', x_s, '</cite>')
citeval <- function(x_s) paste0('<cite class="ea">', x_s, '</cite>')
citesection <- function(x_s) paste0('<cite class="bj">', x_s, '</cite>')
citecode <- function(x_s) paste0('<cite class="oc">', x_s, '</cite>')
citechar <- function(x_s) paste0('<cite class="isa">', x_s, '</cite>')
comment <- function(x_s) paste0('<cite class="comment">', x_s, '</cite>')
citeexr <- function(x_s) paste0('<cite class="bj">', x_s, '</cite>')
citefile <- function(x_s) paste0('<cite class="ff">', x_s, '</cite>')

rdoc <- citefun('wyz.code.rdoc')
roxy <- citefun('roxygen2')
op <- citefun('wyz.code.offensiveProgramming')
R <- citefun('R')

brkfun <- function(x_s) {
  paste(sapply(x_s, function(e) paste('\u25b6', e, '<br/>')), collapse = '')
}

library("data.table")
library("wyz.code.offensiveProgramming")


