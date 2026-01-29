 # Packages ----

 

library(quantmod)

 

# Données ----

 

getSymbols("LMT", src = "yahoo",
           from = as.Date("2021-01-01"),
           to   = as.Date("2025-12-31"))

class(LMT)
head(LMT)
dim(LMT)
price <- LMT[,6] # On prend 6 et pas 4 car il faut enlever les dividendes
 
  

 

