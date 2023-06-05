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

hist(rowMeans(train_roi))

train_roi_belle = train_roi[abs(rowMeans(train_roi)) <= 2, ]
train_info_belle = train_info[abs(rowMeans(train_roi)) <= 2, ]

# Extract the roi names
roi <- unique(
  sub(".*_", "", colnames(train_roi))
)

# Scale
# 1)
train_roi_scale = data.frame(t(apply(train_roi, 1, scale)))

# 2)
# seq(1, 13340, by = 115)
# train_roi[i, j:j+114]

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

sd_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=sd)
}))

mean_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=mean)
}))

# cor_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
#   end <- start + 115 - 1
#   apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=cor)
# }))

max_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=max)
}))

min_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=min)
}))



roi <- unique(
  sub(".*_", "", colnames(train_roi))
)

# Ora posso inserirle nell'altra tabella:
colnames(q1_roi) = paste0('q1_', roi) # molto meglio
colnames(q2_roi) = paste0('q2_', roi)
colnames(q3_roi) = paste0('q3_', roi)
colnames(sd_roi) = paste0('sd_', roi)
colnames(mean_roi) = paste0('mean_', roi)
#colnames(cor_roi) = paste0('cor_', roi)
colnames(max_roi) = paste0('max_', roi)
colnames(min_roi) = paste0('min_', roi)

#train_medie <- as.data.frame(medie_roi)

prova = cbind(train_info, q1_roi, q2_roi, q3_roi)
prova = prova[, -1]
rm(train)

prop.table(table(prova$sex, prova$y))

prova$sex = as.factor(ifelse(prova$sex == 'male', 1, 0))
prova$y = as.factor(ifelse(prova$y == 'autism', 1, 0))


# prop.table(table(prova$y))

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




# PCA ---------------------------------------------------------------------

pca = prcomp(train_roi_scale)
summary(pca)
pca_dataset = data.frame(pca$x)
PCA = cbind(train_info, pca_dataset)
PCA = PCA[, -1]
PCA = PCA[, 1:400]

plot(cumsum(pca$sdev^2)/sum(pca$sdev^2), xlab = "Numero di componenti principali", ylab = "Varianza spiegata cumulativa", type = 'l')
abline(h = 0.85, col = 'red')


# Modello: Random Forest --------------------------------------------------

library(caret)
control <- trainControl(method='cv', 
                        number = 5)

metric <- "Accuracy"
set.seed(123)
mtry <- round(sqrt(ncol(prova) - 1)) # optimal number of columns

tunegrid <- expand.grid(.mtry=c(6, 9, 15, 19, 21, 15)) #c(6, 9, 15, 19, 21, 15)
parameters <- expand.grid(ntrees = c(100, 250, 500),
                          nodesize = c(1, 3, 5))

rf_default <- train(y ~. , 
                    data = prova, 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    tuneGrid = tunegrid,
                    #ntree = 1000,
                    trControl = control)
print(rf_default)
plot(rf_default)

baseline_accuracy <- rf_default$results$Accuracy[2]

varImp(rf_default, scale = F)
var_imp = data.frame(varImp(rf_default, scale = F)$importance)
var_imp$variable = rownames(var_imp)
var_imp <- data.frame(var_imp[order(var_imp$Overall, decreasing = TRUE), ])

top_variables = var_imp[1:10, 2]



# LOCO -------------------------------------------------------------------

top_variables

prova_10 <- prova[, c(top_variables, 'y')]

control <- trainControl(method='cv', 
                        number = 5)
tunegrid <- expand.grid(.mtry=9)
rf_default10 <- train(y ~. , 
                    data = prova_10, 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    tuneGrid = tunegrid,
                    #ntree = 1000,
                    trControl = control)
print(rf_default10)

variables = c()
accuracies = c()
new_acc = c()
for(j in 1:(ncol(prova_10) - 1)){
  
  variables <- c(variables, j)
  
  mtry <- round(sqrt(ncol(prova_10) - 1)) # optimal number of columns
  tunegrid <- expand.grid(.mtry=mtry)
  mdl <- train(y ~. , 
               data = prova_10[, -j], 
               method = 'rf', 
               metric = 'Accuracy', 
               tuneGrid = tunegrid, 
               trControl = control)
  
  new_acc <- c(new_acc, mdl$results$Accuracy)
  accuracies <- c(accuracies, baseline_accuracy - mdl$results$Accuracy)
}

data.frame(cbind('Missing Variable' = variables, 'Delta Accuracy' = accuracies), 'New Accuracy' = new_acc)



# Predict -----------------------------------------------------------------

test <- read_csv("test_hw03.csv")
head(test)


test_info <- test[, c(1:3)]
test_roi <- test[, -c(1:3)]

hist(rowMeans(test_roi))

# test_roi_belle = test_roi[abs(rowMeans(test_roi)) <= 2, ]
# test_info_belle = test_info[abs(rowMeans(test_roi)) <= 2, ]

# Extract the roi names
# roi <- unique(
#   sub(".*_", "", colnames(test_roi))
# )

# Scale
# 1)
test_roi_scale = data.frame(t(apply(test_roi, 1, scale)))

# 2)
# seq(1, 13340, by = 115)
# test_roi[i, j:j+114]

q1_roi_test <- as.data.frame(sapply(seq(1, ncol(test_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(test_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=quantile, probs=0.25)
}))

q2_roi_test <- as.data.frame(sapply(seq(1, ncol(test_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(test_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=quantile, probs=0.5)
}))

q3_roi_test <- as.data.frame(sapply(seq(1, ncol(test_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(test_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=quantile, probs=0.75)
}))

sd_roi_test <- as.data.frame(sapply(seq(1, ncol(test_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(test_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=sd)
}))

mean_roi_test <- as.data.frame(sapply(seq(1, ncol(test_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(test_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=mean)
}))

# cor_roi <- as.data.frame(sapply(seq(1, ncol(test_roi_scale), 115), function(start) {
#   end <- start + 115 - 1
#   apply(test_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=cor)
# }))

max_roi_test <- as.data.frame(sapply(seq(1, ncol(test_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(test_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=max)
}))

min_roi_test <- as.data.frame(sapply(seq(1, ncol(test_roi_scale), 115), function(start) {
  end <- start + 115 - 1
  apply(test_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=min)
}))



# roi <- unique(
#   sub(".*_", "", colnames(test_roi))
# )

# Ora posso inserirle nell'altra tabella:
colnames(q1_roi_test) = paste0('q1_', roi) # molto meglio
colnames(q2_roi_test) = paste0('q2_', roi)
colnames(q3_roi_test) = paste0('q3_', roi)
colnames(sd_roi_test) = paste0('sd_', roi)
colnames(mean_roi_test) = paste0('mean_', roi)
#colnames(cor_roi) = paste0('cor_', roi)
colnames(max_roi_test) = paste0('max_', roi)
colnames(min_roi_test) = paste0('min_', roi)

#test_medie <- as.data.frame(medie_roi)

prova = cbind(test_info, q1_roi_test, q2_roi_test, q3_roi_test)
prova = prova[, -1]
# rm(test)

prop.table(table(prova$sex, prova$y))

prova$sex = as.factor(ifelse(prova$sex == 'male', 1, 0))
# prova$y = as.factor(ifelse(prova$y == 'autism', 1, 0))


prova_10 <- prova[, c(top_variables)]

id <- test$id
target = predict(rf_default10, newdata = prova_10)
target = ifelse(target == 1, 'autism', 'control')

preds_g11 = data.frame('id' = id, 'target' = target)
table(preds_g11$target)

write.csv(preds_g11, file = "preds_g11.csv", row.names = FALSE)
