tsbkg <- function(X, save = TRUE, file = 'tsbkg.txt', ...) {
  # colnames(X) must be: 'Time', 'Wavelength (nm)', 'Intensity (mV)', 'SD (mV)', 'Type'
  wv <- unique(X$`Wavelength (nm)`)
  l  <- length(wv)
  i  <- which(diff(X$Type == 'I0') == -1)
  I0 <- X$`Intensity (mV)`[rep(i, each=l*3)-(0:((l*3)-1))]
  I0 <- matrix(rowMeans(matrix(I0, ncol = 3)), nrow = l)
  I0 <- as.data.frame(cbind(wv, I0), row.names = F)
  colnames(I0) <- c('Wavelength (nm)', paste('I0-', 1:(ncol(I0)-1), sep = ''))
  
  if (save == TRUE) {
    if (file == 'tsbkg.txt') {
      write.table(I0, file='tsbgk.txt', row.names = FALSE, ...)
    }
    else {
      write.table(I0, file = file, row.names = FALSE, ...)
    }
  }
  
  return(I0)
}