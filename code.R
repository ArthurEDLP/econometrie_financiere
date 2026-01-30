 # Packages ----

 

library(quantmod)
library(PerformanceAnalytics)
library(rugarch)
library(tseries)
 
 

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
cr2LMT <- crLMT^2

# Graphique des rentabilités et des rentabilités au carré du LMT
par(mfrow=c(2,1))
plot.xts(crLMT, legend.loc = "top", main = "Rentabilités du LMT", col = rainbow(4))
plot.xts(cr2LMT, legend.loc = "top", main = "Rentabilités au carré du LMT", col = "blue")

###### 4) Corrélogrammes (FAC et FAP) des rentabilités et des rentabilités au carré
par(mfrow=c(2,2))

# Stat descriptives (série corrigée LMT)
#********************************************************************************************
acf(coredata(crLMT), main="Return ACF")
pacf(coredata(crLMT), main="Return PACF")
acf(coredata(cr2LMT), main="Squared return ACF")
pacf(coredata(cr2LMT), main="Squared return PACF")

##### 5) Statistiques descriptives sur la série corrigée
library(PerformanceAnalytics)
table.Stats(crLMT * 100)
table.Distributions(crLMT)
table.Autocorrelation(crLMT)

library(fBasics)
stat <- basicStats(coredata(crLMT) * 100)
show(stat)

library(DescTools)
JarqueBeraTest(coredata(crLMT), robust = FALSE, method = "chisq")

Box.test(coredata(crLMT), lag = 10, type = "Box-Pierce", fitdf = 0)
Box.test(coredata(crLMT), lag = 10, type = "Ljung-Box", fitdf = 0)

library(FinTS)
ArchTest(coredata(crLMT), lags = 5)
ArchTest(coredata(crLMT), lags = 10)

################ 7) Modèle et volatilité ##################
# Loi normale ----
y <- crLMT


###########################  GARCH  ############################


spec_GARCH_N = ugarchspec(variance.model=list(model = "sGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
mod_GARCH_N = ugarchfit(data = y, spec = spec_GARCH_N)
mod_GARCH_N

# Conditionnal variance
return_var_GARCH_N <- xts(mod_GARCH_N@fit$var, order.by = as.Date(index(crLMT)))
plot(return_var_GARCH_N, main = "Variance conditionnelle du modèle GARCH", col = "blue")

par(mfrow=c(2,1))
plot.xts(cr2LMT,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_GARCH_N, main = "Variance conditionnelle du modèle GARCH", col = "blue")

##########################  IGARCH  ############################

spec_IGARCH_N = ugarchspec(variance.model=list(model = "iGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
mod_IGARCH_N = ugarchfit(data = y, spec = spec_IGARCH_N)
mod_IGARCH_N

return_var_IGARCH_N <- xts(mod_IGARCH_N@fit$var, order.by = as.Date(index(crLMT)))

par(mfrow=c(2,1))
plot.xts(cr2LMT,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_IGARCH_N, main = "Variance conditionnelle du modèle IGARCH", col = "blue")

##########################  Riskmetrics  ##############################


spec_RISK_N = ugarchspec(variance.model=list(model = "iGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))
mod_RISK_N = ugarchfit(data = y, spec = spec_RISK_N)
mod_RISK_N

return_var_RISK_N <- xts(mod_RISK_N@fit$var, order.by = as.Date(index(crLMT)))

par(mfrow=c(2,1))
plot.xts(cr2LMT,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_RISK_N, main = "Variance conditionnelle du modèle Riskmetrics", col = "blue")

##########################  GJR-GARCH  ############################


spec_GJR_N = ugarchspec(variance.model=list(model = "gjrGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
mod_GJR_N = ugarchfit(data = y, spec = spec_GJR_N)
mod_GJR_N

return_var_GJR_N <- xts(mod_GJR_N@fit$var, order.by = as.Date(index(crLMT)))

par(mfrow=c(2,1))
plot.xts(cr2LMT,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_GJR_N, main = "Variance conditionnelle du modèle GJR(1,1)", col = "blue")


# Loi Student ----


###########################  GARCH  ############################


spec_GARCH_S = ugarchspec(variance.model=list(model = "sGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                          distribution.model = "std")
mod_GARCH_S = ugarchfit(data = y, spec = spec_GARCH_S)
mod_GARCH_S

return_var_GARCH_S <- xts(mod_GARCH_S@fit$var, order.by = as.Date(index(crLMT)))
plot(return_var_GARCH_S, main = "Variance conditionnelle du modèle GARCH", col = "blue")

par(mfrow=c(2,1))
plot.xts(cr2LMT,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_GARCH_S, main = "Variance conditionnelle du modèle GARCH", col = "blue")

##########################  IGARCH  ############################

spec_IGARCH_S = ugarchspec(variance.model=list(model = "iGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                           distribution.model = "std")
mod_IGARCH_S = ugarchfit(data = y, spec = spec_IGARCH_S)
mod_IGARCH_S

return_var_IGARCH_S <- xts(mod_IGARCH_S@fit$var, order.by = as.Date(index(crLMT)))

par(mfrow=c(2,1))
plot.xts(cr2LMT,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_IGARCH_S, main = "Variance conditionnelle du modèle IGARCH", col = "blue")

##########################  Riskmetrics  ##############################


spec_RISK_S = ugarchspec(variance.model=list(model = "iGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model = "std",
                         fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))
mod_RISK_S = ugarchfit(data = y, spec = spec_RISK_S)
mod_RISK_S

return_var_RISK_S <- xts(mod_RISK_S@fit$var, order.by = as.Date(index(crLMT)))

par(mfrow=c(2,1))
plot.xts(cr2LMT,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_RISK_S, main = "Variance conditionnelle du modèle Riskmetrics", col = "blue")

##########################  GJR-GARCH  ############################


spec_GJR_S = ugarchspec(variance.model=list(model = "gjrGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                        distribution.model = "std")
mod_GJR_S = ugarchfit(data = y, spec = spec_GJR_S)
mod_GJR_S

return_var_GJR_S <- xts(mod_GJR_S@fit$var, order.by = as.Date(index(crLMT)))

par(mfrow=c(2,1))
plot.xts(cr2LMT,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_GJR_S, main = "Variance conditionnelle du modèle GJR(1,1)", col = "blue")




