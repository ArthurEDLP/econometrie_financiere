 # Packages ----

 

library(quantmod)
library(PerformanceAnalytics)
library(rugarch)
library(tseries)
library(fBasics)
library(FinTS)
library(DescTools) 
 

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
##### 6) Caractéristiques de distribution sur la série corrigée. Résultats sous forme de tableau. Commenter

library(PerformanceAnalytics)

table.Stats(crLMT * 100)
table.Distributions(crLMT)
table.Autocorrelation(crLMT)

stat <- basicStats(coredata(crLMT) * 100)
show(stat)


JarqueBeraTest(coredata(crLMT), robust = FALSE, method = "chisq")

Box.test(coredata(crLMT), lag = 10, type = "Box-Pierce", fitdf = 0)
Box.test(coredata(crLMT), lag = 10, type = "Ljung-Box", fitdf = 0)


ArchTest(coredata(crLMT), lags = 5)
ArchTest(coredata(crLMT), lags = 10)

################ 7) Modèle et volatilité ##################
# Loi normale ----

# Limiter à fin 2024
crLMT_2024 <- crLMT["/2024-12-31"]


y_2024 <- crLMT_2024 
  
cr2LMT_2024 <- cr2LMT["/2024-12-31"]

###########################  GARCH  ############################


spec_GARCH_N = ugarchspec(variance.model=list(model = "sGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
mod_GARCH_N = ugarchfit(data = y_2024, spec = spec_GARCH_N)
mod_GARCH_N

# Conditionnal variance
return_var_GARCH_N <- xts(mod_GARCH_N@fit$var, order.by = as.Date(index(crLMT_2024)))
plot(return_var_GARCH_N, main = "Variance conditionnelle du modèle GARCH", col = "blue")

par(mfrow=c(2,1))
plot.xts(cr2LMT_2024,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_GARCH_N, main = "Variance conditionnelle du modèle GARCH (normal)", col = "blue")

##########################  IGARCH  ############################

spec_IGARCH_N = ugarchspec(variance.model=list(model = "iGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
mod_IGARCH_N = ugarchfit(data = y_2024, spec = spec_IGARCH_N)
mod_IGARCH_N

return_var_IGARCH_N <- xts(mod_IGARCH_N@fit$var, order.by = as.Date(index(crLMT_2024)))

par(mfrow=c(2,1))
plot.xts(cr2LMT_2024,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_IGARCH_N, main = "Variance conditionnelle du modèle IGARCH (normal)", col = "blue")

##########################  Riskmetrics  ##############################


spec_RISK_N = ugarchspec(variance.model=list(model = "iGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))
mod_RISK_N = ugarchfit(data = y_2024, spec = spec_RISK_N)
mod_RISK_N

return_var_RISK_N <- xts(mod_RISK_N@fit$var, order.by = as.Date(index(crLMT_2024)))

par(mfrow=c(2,1))
plot.xts(cr2LMT_2024,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_RISK_N, main = "Variance conditionnelle du modèle Riskmetrics (normal)", col = "blue")

##########################  GJR-GARCH  ############################


spec_GJR_N = ugarchspec(variance.model=list(model = "gjrGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
mod_GJR_N = ugarchfit(data = y_2024, spec = spec_GJR_N)
mod_GJR_N

return_var_GJR_N <- xts(mod_GJR_N@fit$var, order.by = as.Date(index(crLMT_2024)))

par(mfrow=c(2,1))
plot.xts(cr2LMT_2024,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_GJR_N, main = "Variance conditionnelle du modèle GJR(1,1) (normal)", col = "blue")


# Loi Student ----


###########################  GARCH  ############################


spec_GARCH_S = ugarchspec(variance.model=list(model = "sGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                          distribution.model = "std")
mod_GARCH_S = ugarchfit(data = y_2024, spec = spec_GARCH_S)
mod_GARCH_S

return_var_GARCH_S <- xts(mod_GARCH_S@fit$var, order.by = as.Date(index(crLMT_2024)))

par(mfrow=c(2,1))
plot.xts(cr2LMT_2024,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_GARCH_S, main = "Variance conditionnelle du modèle GARCH (student)", col = "blue")

##########################  IGARCH  ############################

spec_IGARCH_S = ugarchspec(variance.model=list(model = "iGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                           distribution.model = "std")
mod_IGARCH_S = ugarchfit(data = y_2024, spec = spec_IGARCH_S)
mod_IGARCH_S

return_var_IGARCH_S <- xts(mod_IGARCH_S@fit$var, order.by = as.Date(index(crLMT_2024)))

par(mfrow=c(2,1))
plot.xts(cr2LMT_2024,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_IGARCH_S, main = "Variance conditionnelle du modèle IGARCH (student)", col = "blue")

##########################  Riskmetrics  ##############################


spec_RISK_S = ugarchspec(variance.model=list(model = "iGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                         distribution.model = "std",
                         fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))
mod_RISK_S = ugarchfit(data = y_2024, spec = spec_RISK_S)
mod_RISK_S

return_var_RISK_S <- xts(mod_RISK_S@fit$var, order.by = as.Date(index(crLMT_2024)))

par(mfrow=c(2,1))
plot.xts(cr2LMT_2024,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_RISK_S, main = "Variance conditionnelle du modèle Riskmetrics (student)", col = "blue")

##########################  GJR-GARCH  ############################


spec_GJR_S = ugarchspec(variance.model=list(model = "gjrGARCH"), mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                        distribution.model = "std")
mod_GJR_S = ugarchfit(data = y_2024, spec = spec_GJR_S)
mod_GJR_S

return_var_GJR_S <- xts(mod_GJR_S@fit$var, order.by = as.Date(index(crLMT_2024)))

par(mfrow=c(2,1))
plot.xts(cr2LMT_2024,legend.loc = "top", main = "Rentabilités au carré du LOCKHEED", col = rainbow(4))
plot.xts(return_var_GJR_S, main = "Variance conditionnelle du modèle GJR(1,1) (student)", col = "blue")

##########################  Persistence  ############################

# Persistence ----

# Compute persistence

##### Loi normal

pers_GARCH_N = persistence(mod_GARCH_N) 
show(pers_GARCH_N)

pers_IGARCH_N = persistence(mod_IGARCH_N) 
show(pers_IGARCH_N)

pers_RISK_N = persistence(mod_RISK_N) 
show(pers_RISK_N)

pers_GJR_N = persistence(mod_GJR_N) 
show(pers_GJR_N)

###### Loi de student

pers_GARCH_S = persistence(mod_GARCH_S) 
show(pers_GARCH_S)

pers_IGARCH_S = persistence(mod_IGARCH_S) 
show(pers_IGARCH_S)

pers_RISK_S = persistence(mod_RISK_S) 
show(pers_RISK_S)

pers_GJR_S = persistence(mod_GJR_S) 
show(pers_GJR_S)

##########################  Demi-vie  ############################

# Demi-vie ----

# Compute half-life


##### Loi normal

hl_GARCH_N = halflife(mod_GARCH_N) 
show(hl_GARCH_N)

hl_IGARCH_N = halflife(mod_IGARCH_N) 
show(hl_IGARCH_N)

hl_RISK_N = halflife(mod_RISK_N) 
show(hl_RISK_N)

hl_GJR_N = halflife(mod_GJR_N) 
show(hl_GJR_N)

###### Loi de student

hl_GARCH_S = halflife(mod_GARCH_S) 
show(hl_GARCH_S)

hl_IGARCH_S = halflife(mod_IGARCH_S) 
show(hl_IGARCH_S)

hl_RISK_S = halflife(mod_RISK_S) 
show(hl_RISK_S)

hl_GJR_S = halflife(mod_GJR_S) 
show(hl_GJR_S)

##################### Différents tests #########################

# résidus standardisés

z_IGARCH_S <- residuals(mod_IGARCH_S, standardize = TRUE)
z_GARCH_N <- residuals(mod_GARCH_N, standardize = TRUE)

# autocorr des résidus

Box.test(as.numeric(z_GARCH_N), lag = 20, type = "Ljung-Box")
Box.test(as.numeric(z_GARCH_N)^2, lag = 20, type = "Ljung-Box")

###############
## IGARCH_S
###############

Box.test(as.numeric(z_IGARCH_S), lag = 20, type = "Ljung-Box")
Box.test(as.numeric(z_IGARCH_S)^2, lag = 20, type = "Ljung-Box")


######################### VAR ##################################
## VAR ----


b <- nrow(crLMT)	# b=1254
estim <- nrow(crLMT_2024)		# nbre observations des rentabilités de 2021 à 2024 = 1005
h <- b-estim+1		# nbre observations des rentabilités en 2025
original <- crLMT[estim:b, 1]		# rentabilités originales en 2024

y <- crLMT

#----------------------------------------------------------
# Matrix initialization
foremat <- matrix(nrow=h, ncol=1)			# matrice contenant les prévisions de la variance
varmat <- matrix(nrow=h, ncol=1)			# matrice contenant les prévisions de la VaR
esmat <- matrix(nrow=h, ncol=1)

#----------------------------------------------------------
# GARCH
for(i in 1:h)
{
  yy <- crLMT[i:(estim-1+i),1]
  fit = ugarchfit(data = yy, spec = spec_GARCH_N)
  forc =  ugarchforecast(fit, n.ahead=1)
  foremat[i,1] <- sigma(forc)^2
  varmat[i,1] <- qnorm(0.05)*sigma(forc)
  esmat[i,1] <- -dnorm(qnorm(0.05))/0.05*sigma(forc)
}

# IGARCH
for(i in 1:h)
{
  yy <- crLMT[i:(estim-1+i),1]
  fit = ugarchfit(data = yy, spec = spec_IGARCH_S)
  forc =  ugarchforecast(fit, n.ahead=1)
  foremat[i,1] <- sigma(forc)^2
  varmat[i,1] <- qnorm(0.05)*sigma(forc)
  esmat[i,1] <- -dnorm(qnorm(0.05))/0.05*sigma(forc)
}

error <- original^2 - foremat
mse <- mean(error^2)
mse*100			# MSE(%)
mean(varmat)	# VaR moyenne
mean(esmat)		# Expected shortfall

# VaR figure
par(mfrow=c(1,1))
data <- cbind(original,varmat)
options(repr.plot.res = 300, repr.plot.height = 4.4) 
plot.xts(data,legend.loc = "topleft", main = "Returns and VaR", col = rainbow(4))
 

 