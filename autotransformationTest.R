source('autotransfuncQF.R')
source('ADStatQF.R')
source('AutoTransQF.R')
library(nortest)

autotransfuncQFtest = function(data){
  par(mfrow = c(2, 2))
  hist(data)
  qqnorm(data, main = "qqplot before transformation")
  qqline(data)
  
  transformed.data = unlist(AutoTransQF(t(data), paramstruct = list(iscreenwrite = 0))[1])
  beta = unlist(AutoTransQF(t(data), paramstruct = list(iscreenwrite = 0))[2])
  alpha = unlist(AutoTransQF(t(data), paramstruct = list(iscreenwrite = 0))[3])
  
  hist(transformed.data)
  qqnorm(transformed.data, main = "qqplot after transformation")
  qqline(transformed.data)
  
  cat(paste('The Anderson-Darling Test Statistic for original data is:', ADStatQF(data)), sep = "",'\n')
  cat(paste('The p-value for Anderson-Darling Test on original data is:', ad.test(data)$p.value), sep = "",'\n')
  cat(paste('The Anderson-Darling Test Statistic for transformed data is:', ADStatQF(transformed.data)), sep = "",'\n')
  cat(paste('The p-value for Anderson-Darling Test on transformed data is:', ad.test(transformed.data)$p.value), sep = "",'\n')
  cat(paste('The shift parameter beta is:', beta, sep = "", '\n'))
  cat(paste('The shift parameter alpha is:', alpha, sep = "", '\n'))
}

ADStatQFtest = function(data) {
  par(mfrow = c(1, 2))
  hist(data, main = "Distribution for Data")
  qqnorm(data, main = "qqplot before transformation")
  qqline(data)
  
  cat(paste('The Anderson-Darling Test Statistic is:', ADStatQF(data)), sep = "",'\n')
  cat(paste('The p-value for Anderson-Darling Test is:', ad.test(data)$p.value), sep = "",'\n')
}
