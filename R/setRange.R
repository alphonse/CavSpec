setRange <- function(X, xmins=c(350, 415, 515, 660), xmaxs=c(400, 465, 565, 700), na.column=1) {
  l <- length(xmins)
  
  # filter out non-enhanced X and replace with NA:
  s <- matrix(rep(X$lambda, each=l) > xmins & rep(X$lambda, each=l) < xmaxs, nrow = 4) %>%
    colSums()
  i <- which(s == 1)
  X[-i, na.column] <- NA
  
  X
}