batchImport <- function(sample.path, ...) {
  list.files(path = sample.path, pattern = '*.txt', full.names=TRUE) %>%
    lapply(FUN = read.table, ...) %>%
    setNames(as.character(strsplit(list.files(
      path = sample.path, pattern = '*.txt'), split = '.txt'))) %>% 
    ldply() %>%
    subset(select = c(1, 4, 5)) %>%
    setNames(c('Gas', 'lambda', 'I'))
}