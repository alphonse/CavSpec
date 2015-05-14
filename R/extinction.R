# Contains function to calculate extinction from cavity techniques. (7/15/2013--AF)

# Units: ------------------------------
#   - I/I0: arbitrary
#        - if I/I0 = phi/phi0, units = degrees
#   - d/Rl: cm
#   - c: cm/sec
#   - alpha: cm^-1
#   - R: arbitrary (should be <1)
#   - w: rad/sec

# Code ---------------------------------
extinction <- function(I, I0, method = "BB", R = 0.9998, Rl = 1.33, d = 50.3, f = 100000, bg.ext = 0){

# define constants -----
c <- 2.99e10  # speed of light
w <- 2 * pi * f  # convert from Hz to rad/s

# CEAS calculator -----
if (method == "BB") {

alpha <- Rl * ((1-R)/d + bg.ext) * ((I0-I)/I)
}

# PS-CRDS calculator -----
else if (method == "PS") {
  alpha = Rl * (-w/c) * (1/tan((pi/180)*(I)) - 1/tan((pi/180)*(I0)))
}

# CRDS calculator -----
else if (method == "CRD") {
  alpha <- "Need eqn. for CRDS extinction!"
}

# Message if method is unknown -----
else {
  alpha <- "Method unknown! Use one of the following methods: PS (phase-shift), BB (broadband cavity enahnced), or CRDS (typ. cavity ringdown)."
}

return(alpha)
}