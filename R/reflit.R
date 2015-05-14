reflit <- function(lambda, T = 273, p = 101325) {
  l <- (p/(1.3806488e-23 * T))/100^3
  lit.data <- list(
    CO2 = splinefun(litSpectrum('CO2', version = 'Sneep')),
    He  = splinefun(litSpectrum('He',  version = 'Washenfelder')),
    N2  = splinefun(litSpectrum('N2',  version = 'Washenfelder')),
    NO2 = splinefun(litSpectrum('NO2', version = 'Bogumil'))
  )
lit.data <- data.frame(
    CO2 = lit.data[['CO2']](lambda) * l,
    He  = lit.data[['He']](lambda)  * l,
    N2  = lit.data[['N2']](lambda)  * l,
    NO2 = lit.data[['NO2']](lambda) * l
  ) %>% stack() %>%
  setNames(c('alpha', 'Gas'))
}
