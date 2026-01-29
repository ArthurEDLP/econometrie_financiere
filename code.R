 # Packages ----

 

library(quantmod)
library(PerformanceAnalytics)
 
 

# Données ----

 
getSymbols("LMT", src = "yahoo",
           from = as.Date("2021-01-01"),
           to   = as.Date("2025-12-31"))

head(LMT)
price <- LMT[,6] # On prend 6 et pas 4 car il faut enlever les dividendes


################ 2) Outiliers et valeurs impotante ##################


# VISUALISATION ET HISTOGRAMME 
rLMT <- dailyReturn(price$LMT.Adjusted)

crLMT <- Return.clean(rLMT, method = "boudt")

write(t(crLMT), file = "clean-returns_LMT.out", ncolumn = 1, append = FALSE)

data_ret <- cbind(crLMT, rLMT)
options(repr.plot.res = 300, repr.plot.height = 4.4)
plot.xts(data_ret, legend.loc = "top",
         main = "Clean and raw LMT returns", col = rainbow(4))

par(mfrow = c(1, 2))
chart.Histogram(rLMT, xlab = "Returns", ylab = "Frequency",
                methods = c("add.density", "add.normal"))
chart.Histogram(crLMT, xlab = "Clean returns", ylab = "Frequency",
                methods = c("add.density", "add.normal"))

# SKEW et KURT 
table_stats <- rbind(
  Raw   = c(skewness(rLMT), kurtosis(rLMT)), # -0.9301459 12.859495
  Clean = c(skewness(crLMT), kurtosis(crLMT)) # -0.2930737  2.706844
)

colnames(table_stats) <- c("Skewness", "Kurtosis")
table_stats

# BOXPLOT ET VISUALISATION 
par(mfrow = c(1, 2))
boxplot(rLMT,  main = "Raw returns",   col = "lightgray")
boxplot(crLMT, main = "Clean returns", col = "lightgray")

# JARQUE BERA POUR LE REJET DE LA NORMALITÉ SI BCP DE VALEUR ATYPIQUE 
library(tseries)

jarque.bera.test(coredata(rLMT)) # X-squared = 8855.7, df = 2, p-value < 2.2e-16
jarque.bera.test(coredata(crLMT)) # X-squared = 403.41, df = 2, p-value < 2.2e-16

#DETECTER LE TOP 3 

# 3) Faire les graphiques

 
## Visualisation des données ----

options(repr.plot.res = 300, repr.plot.height = 4.4) 
plot.xts(LMT[,1:4],legend.loc = "left", main = "Prix de l'action LOCKHEED", col = rainbow(4))


## Calcul des rentabilités ----
return <- dailyReturn(price)
creturn <- Return.clean(return, method = "boudt") # base de données corrigée


# Graphique du cours du cloture et des rentabilités 
par(mfrow=c(2,1))
plot.xts(LMT[,6],legend.loc = "topleft", main = "Cours de l'indice LMT", col = rainbow(4))
plot.xts(return,legend.loc = "topleft", main = "Rentabilités du LMT", col = "blue")


par(mfrow=c(1,1))
data <- cbind(creturn,return)
options(repr.plot.res = 300, repr.plot.height = 4.4) 
plot.xts(data,legend.loc = "top", main = "Clean and raw LMT returns", col = rainbow(4))

# Calcul des rentabilités au carré du LMT
creturn2 <- creturn^2

# Graphique des rentabilités et des rentabilités au carré du LMT
par(mfrow=c(2,1))
plot.xts(creturn,legend.loc = "top", main = "Rentabilités du LMT", col = rainbow(4))
plot.xts(creturn2,legend.loc = "top", main = "Rentabilités au carré du LMT", col = "blue")



















