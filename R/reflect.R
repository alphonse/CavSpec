# Contains function to calculate mirror reflectivity based on PSCRDS measurement. Assumes cavity is filled uniformly with single extinction (i.e. purge flow is same gas as cavity gas).  (7/15/2013--AF)

# Units: ------------------------------
#   - phi: degrees
#   - d: cm
#   - c: cm/sec
#   - alpha: cm^-1
#   - R: arbitrary (should be <1)
#   - w: rad/sec

# Code ---------------------------------

reflect <- function(I, I0, d = 53.3, f = 100000, alpha = 3.9e-7, method = "BB", Rl = 1) {

  if (method == "PS") {
  c <- 2.99e10
  w <- 2 * f * pi
  R <- 1 + d * (w / (c * tan((pi/180) * I)) + alpha)
  }
  
  if (method == "BB") {
    R <- 1 - ((d * alpha) * (I/(I0 - I)))
  }
  
  return(R)
}