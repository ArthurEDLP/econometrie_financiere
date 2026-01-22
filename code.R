

data <- read.table("LOCKHEEDMARTIN.txt", header = TRUE, sep = "", stringsAsFactors = FALSE)

summary(data)

str(data)

data[c("ouv", "haut", "bas", "vol", "devise")] <- NULL
