#     Statistical Learning Homework 2, group 11
# Susanna Bravi, Simone Facchiano, Raffaele Liguori



# 0 - Packages --------------------------------------------------------

library(readr)


# 1 - Initialization ------------------------------------------------------

setwd("~/Desktop/Data Science/SL/Homework 2")
train <- read_csv("train_hw03.csv")


# 2 - Preprocessing -------------------------------------------------------

# Primo problema: l'ordine delle colonne
# Secondo problema: i valori (alcuni intorno allo 0, altri del tutto sballati)
hist(rowMeans(train[, -c(1:4)]), breaks = 20) # ad esempio, qui la seconda riga ha media -3, mentre tutte le altre sono intorno allo 0

# Creaiamo una copia del dataset, ma senza le prime 4 colonne
train_info <- train[, c(1:4)]
train_roi <- train[, -c(1:4)]

# Extract the roi names
roi <- unique(
  sub(".*_", "", colnames(train_roi))
)

train_roi_scale = data.frame(t(apply(train_roi, 1, scale)))

q1_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=quantile, probs=0.25)
}))

q2_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=quantile, probs=0.5)
}))

q3_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=quantile, probs=0.75)
}))


#train_medie <- as.data.frame(medie_roi)

# Tutto bello, ma i nomi non sono indicativi. Dobbiamo trovare un modo per estrarre i nomi delle ROI, così possiamo rendere questa tabella più informativa.


# 2.1 - Estrarre i nomi delle ROI -----------------------------------------

# Estraggo i nomi delle colonne usando colnames(train_roi).
# Uso la funzione sub + una regex per eliminare tutto ciò che c'è prima del trattino.
# Uso unique per estrarre i valori univoci
roi <- unique(
  sub(".*_", "", colnames(train_roi))
  )

# Ora posso inserirle nell'altra tabella:
colnames(q1_roi) = paste0('q1_', roi) # molto meglio
colnames(q2_roi) = paste0('q2_', roi)
colnames(q3_roi) = paste0('q3_', roi)

# Ora rimetto tutto insieme:

train <- cbind(train_info, q1_roi, q2_roi, q3_roi)

# E cancelliamo le cose inutili a questo punto 
# rm(train_roi)
# rm(train_medie)
# rm(train_info)
# rm(medie_roi)

# Standardizzazione dei dati (per riga) ----------------------------------------------------------

hist(rowMeans(train[, -c(1:4)])) # easy