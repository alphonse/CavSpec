litSpectrum <- function(species, version = NULL) {

lit.colnames <- c('lambda', 'sigma')

# Pull O3 data from web and format -----
if (species == 'O3') {
O3.burrows.url <- 'http://joseba.mpch-mainz.mpg.de/spectral_atlas_data/cross_sections/Ozone/O3_Burrows%281999%29_293K_230-794nm%28air%29.txt'
O3.burrows <- read.csv(O3.burrows.url, header = F, sep = ' ')
O3.burrows <- O3.burrows[ , -2:-5]
colnames(O3.burrows) <- lit.colnames
return(O3.burrows)
}

# Pull NO2 data from web and format -----
if (species == 'NO2') { 
  if (version == 'Bogumil') {
  NO2.bogumil.url <- 'http://joseba.mpch-mainz.mpg.de/spectral_atlas_data/cross_sections/Nitrogen%20oxides/NO2_Bogumil%282003%29_293K_230-930nm.txt'
  NO2.bogumil <- read.csv(NO2.bogumil.url, header = F, sep = ' ')
  NO2.bogumil <- NO2.bogumil[ , -2:-5]
  colnames(NO2.bogumil) <- lit.colnames
  return(NO2.bogumil)
  }

# Pull NO2 data from web and format -----
  if (version == 'Davidson') { 
  NO2.david.url <- 'http://joseba.mpch-mainz.mpg.de/spectral_atlas_data/cross_sections/Nitrogen%20oxides/NO2_Davidson%281988%29_298K_263-649nm.txt'
  NO2.david <- read.csv(NO2.david.url, header = F, sep = ' ')
  NO2.david <- NO2.david[ , -2:-4]
  colnames(NO2.david) <- lit.colnames
  return(NO2.david)
}}

# Calculate CO2 spectrum according to NASA fit -----
if (species == 'CO2') {
  if (version == 'NASA') {
    lambda <- seq(200, 1200, by = 0.1)
    CO2.nasa <- 1.943e-24*exp(-9.113e-3 * lambda)
    CO2.nasa <- data.frame(lambda = lambda, sigma = CO2.nasa)
    return(CO2.nasa)
  }
  
  if (version == 'Sneep') {
    lambda <- seq(200, 1200, by = 0.1)
    nu <- (lambda * 1e-7)^-1
    CO2.sneep <- (27.42*1e-47) * nu^(4+0.1343) * 100
    CO2.sneep <- data.frame(lambda = lambda, sigma = CO2.sneep)
    return(CO2.sneep)
  }
}

# Calculate N2 spectrum according to Washenfelder 2013 -----
if (species == 'N2') {
  lambda <- seq(200, 1200, by = 0.1)
  N2.washenfelder <- 1.2577e-15 * lambda^-4.1814
  N2.washenfelder <- data.frame(lambda = lambda, sigma = N2.washenfelder)
  return(N2.washenfelder)
}

# Calculate He spectrum according to Washenfelder 2013 -----
if (species == 'He') {
  lambda <- seq(200, 1200, by = 0.1)
  He.washenfelder <- 1.336e-17 * lambda^-4.1287
  He.washenfelder <- data.frame(lambda = lambda, sigma = He.washenfelder)
  return(He.washenfelder)
}


# Calculate O2 spectrum according to NASA -----
if (species == 'O2') {
  lambda <- seq(200, 1200, by = 0.1)
  O2.rayleigh.nasa <- 4.1214e-25*exp(-0.0085 * lambda)
  O2.rayleigh.nasa <- data.frame(lambda = lambda, sigma = O2.rayleigh.nasa)
  return(O2.rayleigh.nasa)
}

# Pull O4 spectrum from web and format -----
if (species == 'O4') {
  O4.volkamer.url <- 'http://joseba.mpch-mainz.mpg.de/spectral_atlas_data/cross_sections/Oxygen/O4_Volkamer%281996%29_296K_360.5-630.0nm%28max%29.txt'
  O4.volkamer <- read.csv(O4.volkamer.url, header = T, sep = '\t', row.names = NULL)
  O4.volkamer <- O4.volkamer[ , -3:-4]
  colnames(O4.volkamer) <- lit.colnames
  return(O4.volkamer)
}
}