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

# Valeur absolue des rendements bruts
abs_r <- abs(coredata(rLMT))

# Indices des 3 plus grands chocs
idx_top3 <- order(abs_r, decreasing = TRUE)[1:3]

# Tableau récapitulatif
top3_outliers <- data.frame(
  date = index(rLMT)[idx_top3],
  raw_return   = coredata(rLMT)[idx_top3],
  clean_return = coredata(crLMT)[idx_top3]
)

top3_outliers


# 3) Faire les graphiques

################ 3) Graphiques ##################

 
## Visualisation des données ----

options(repr.plot.res = 300, repr.plot.height = 4.4) 
plot.xts(LMT[,1:4],legend.loc = "left", main = "Prix de l'action LOCKHEED", col = rainbow(4))


# Graphique du cours du cloture et des rentabilités 
par(mfrow=c(2,1))
plot.xts(LMT[,6],legend.loc = "topleft", main = "Cours de l'indice LMT", col = rainbow(4))
plot.xts(rLMT,legend.loc = "topleft", main = "Rentabilités du LMT", col = "blue")


par(mfrow=c(1,1))
data <- cbind(crLMT,rLMT)
options(repr.plot.res = 300, repr.plot.height = 4.4) 
plot.xts(data,legend.loc = "top", main = "Clean and raw LMT returns", col = rainbow(4))

creturn <- crLMT
# Calcul des rentabilités au carré du LMT
creturn2 <- creturn^2

# Graphique des rentabilités et des rentabilités au carré du LMT
par(mfrow=c(2,1))
plot.xts(creturn,legend.loc = "top", main = "Rentabilités du LMT", col = rainbow(4))
plot.xts(creturn2,legend.loc = "top", main = "Rentabilités au carré du LMT", col = "blue")


###### 4) Faire les graphiques des corrélagrammes (FAC et FAP) des rentabilités et des rentabilités au carré (série corrigée). Commenter

# Corrélogrammes des rentabilités
par(mfrow=c(2,2))
acf(creturn, main="Return ACF")
pacf(creturn, main="Return PACF")
acf(creturn2, main="Squared return ACF")
pacf(creturn2, main="Squared return PACF")

##### 5) Statistiques descriptives sur la série corrigée. Résultats sous forme de tableau.Commenter
##### 6) Caractéristiques de distribution sur la série corrigée. Résultats sous forme de tableau. Commenter

# Stat descriptives (série corrigée LMT)
#********************************************************************************************

library(PerformanceAnalytics)

# creturn = clean returns (ici : crLMT)
creturn <- crLMT

# PerformanceAnalytics (attention: table.Stats donne Ex.Kurtosis)
table.Stats(creturn * 100)
table.Distributions(creturn)
table.Autocorrelation(creturn)

# fBasics : moyenne, sd, skewness, kurtosis, etc.
library(fBasics)
stat1 <- basicStats(coredata(creturn))      # rendements en décimal
show(stat1)

stat <- basicStats(coredata(creturn) * 100) # rendements en %
show(stat)

# Jarque-Bera (DescTools)
library(DescTools)
JarqueBeraTest(coredata(creturn), robust = FALSE, method = "chisq")

# Tests d'autocorrélation Box-Pierce et Ljung-Box (10 retards)
Box.test(coredata(creturn), lag = 10, type = "Box-Pierce", fitdf = 0)
Box.test(coredata(creturn), lag = 10, type = "Ljung-Box", fitdf = 0)

# Test ARCH (FinTS) sur 5 et 10 retards
library(FinTS)
ArchTest(coredata(creturn), lags = 5)
ArchTest(coredata(creturn), lags = 10)












