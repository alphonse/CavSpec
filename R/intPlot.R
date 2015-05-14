intPlot <- function(X) {
  manipulate(
  {
    ggplot(aes(x = lambda, y = I), data = X) +
      geom_line(aes(color = Gas)) + theme_bw() + xlim(xmin, xmax) + ylim(ymin, ymax) +
      xlab(expression(paste(lambda, ' (nm)')))
  },
  ymin=slider(0, max(na.omit(X$I))*1.1, 
              step=5, 
              initial = 0),
  xmin=slider(350, 750,
              step = 1, 
              initial = 350),
  ymax=slider(0, max(na.omit(X$I))*1.1, 
              step=5, 
              initial = max(na.omit(X$I))*1.1),
  xmax=slider(350, 750,
              step = 1, 
              initial = 700)
  )
}