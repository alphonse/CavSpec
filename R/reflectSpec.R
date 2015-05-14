reflectSpec <- function(refData, litData, Rmethod = 'HeCO2', correction = 0) {
  lambda <- unique(refData$lambda)
  if (Rmethod == 'HeCO2') {
    R <- data.frame(
      HeCO2 = reflect(filter(refData, Gas == 'CO2')$I, filter(refData, Gas == 'He')$I,   alpha = filter(litData, Gas == 'CO2')$alpha - filter(litData, Gas == 'He')$alpha)) %>% stack() %>%
      setNames(c('R', 'Gases'))
    R$lambda <- lambda
  }
  
  else if (Rmethod == 'N2CO2') {
    R <- data.frame(
      N2CO2 = reflect(filter(refData, Gas == 'CO2')$I, filter(refData, Gas == 'N2')$I, alpha = filter(litData, Gas == 'CO2')$alpha - filter(litData, Gas == 'N2')$alpha)) %>% 
      stack() %>%
      setNames(c('R', 'Gases'))
    R$lambda <- lambda
  }
  
  else if (Rmethod == 'HeN2') {
    R <- data.frame(
      HeN2 = reflect(filter(refData, Gas == 'N2')$I, filter(refData, Gas == 'He')$I, alpha = filter(litData, Gas == 'N2')$alpha - filter(litData, Gas == 'He')$alpha)) %>% 
      stack() %>%
      setNames(c('R', 'Gases'))
    R$lambda <- lambda
  }
  
  else if (Rmethod == 'all') {
    R <- data.frame(
      HeCO2 = reflect(filter(refData, Gas == 'CO2')$I, filter(refData, Gas == 'He')$I, alpha = filter(litData, Gas == 'CO2')$alpha - filter(litData, Gas == 'He')$alpha),
      HeN2 = reflect(filter(refData, Gas == 'N2')$I, filter(refData, Gas == 'He')$I, alpha = filter(litData, Gas == 'N2')$alpha - filter(litData, Gas == 'He')$alpha),
      N2CO2 = reflect(filter(refData, Gas == 'CO2')$I, filter(refData, Gas == 'N2')$I, alpha = filter(litData, Gas == 'CO2')$alpha - filter(litData, Gas == 'N2')$alpha)) %>% 
      stack() %>%
      setNames(c('R', 'Gases'))
    R$lambda <- lambda
  }
  
  R$R <- R$R + correction
  R
}