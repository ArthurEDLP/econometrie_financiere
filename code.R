 # Packages ----

 

library(quantmod)
library(PerformanceAnalytics)
 
 

# Données ----

 
getSymbols("LMT", src = "yahoo",
           from = as.Date("2021-01-01"),
           to   = as.Date("2025-12-31"))

head(LMT)
price <- LMT[,6] # On prend 6 et pas 4 car il faut enlever les dividendes
 
# Visualisation des données ----

options(repr.plot.res = 300, repr.plot.height = 4.4) 
plot.xts(LMT[,1:4],legend.loc = "left", main = "Prix de l'action LOCKHEED", col = rainbow(4))

# Calcul des rentabilités
return <- dailyReturn(price)
creturn <- Return.clean(return, method = "boudt")

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
