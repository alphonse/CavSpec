refPlot <- function(X) {
  manipulate(
{
  ggplot(aes(x = lambda, y = R), data = X) +
    geom_line(aes(color = Gases)) + theme_bw() + xlim(xmin, xmax) + ylim(ymin, ymax) + 
    xlab(expression(paste(lambda, ' (nm)')))
},
ymin=slider(0.995, 1, 
            step=0.0001, 
            initial = 0.9990),
xmin=slider(350, 750,
            step = 1, 
            initial = 350),
ymax=slider(0, 1, 
            step=0.0001, 
            initial = 1),
xmax=slider(350, 750,
            step = 1, 
            initial = 700)
  )
}