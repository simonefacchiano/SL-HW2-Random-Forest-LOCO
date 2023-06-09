#     Statistical Learning Homework 2, group 11
# Susanna Bravi, Simone Facchiano, Raffaele Liguori



# 0 - Packages --------------------------------------------------------

library(readr)


# 1 - Initialization ------------------------------------------------------

setwd("~/Desktop/Data Science/SL/Homework 2")
train <- read_csv("train_hw03.csv")


# 2 - Preprocessing -------------------------------------------------------

# Problema: i valori (alcuni intorno allo 0, altri del tutto sballati)
hist(rowMeans(train[, -c(1:4)]), breaks = 20) # ad esempio, qui la seconda riga ha media -3, mentre tutte le altre sono intorno allo 0

# Creaiamo una copia del dataset, ma senza le prime 4 colonne
train_info <- train[, c(1:4)]
train_roi <- train[, -c(1:4)]

hist(rowMeans(train_roi), border = F)

# train_roi_belle = train_roi[abs(rowMeans(train_roi)) <= 2, ]
# train_info_belle = train_info[abs(rowMeans(train_roi)) <= 2, ]


# Scale
# 1)
train_roi_scale = data.frame(t(apply(train_roi, 1, scale)))

# 2)

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

# sd_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
#   end <- start + 115 - 1
#   apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=sd)
# }))

# mean_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
#   end <- start + 115 - 1
#   apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=mean)
# }))

# max_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
#   end <- start + 115 - 1
#   apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=max)
# }))
# 
# min_roi <- as.data.frame(sapply(seq(1, ncol(train_roi_scale), 115), function(start) {
#   end <- start + 115 - 1
#   apply(train_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=min)
# }))



roi <- unique(
  sub(".*_", "", colnames(train_roi))
)

# Ora posso inserirle nell'altra tabella:
colnames(q1_roi) = paste0('q1_', roi) # molto meglio
colnames(q2_roi) = paste0('q2_', roi)
colnames(q3_roi) = paste0('q3_', roi)
#colnames(sd_roi) = paste0('sd_', roi)
#colnames(mean_roi) = paste0('mean_', roi)
# colnames(max_roi) = paste0('max_', roi)
# colnames(min_roi) = paste0('min_', roi)


prova = cbind(train_info, q1_roi, q2_roi, q3_roi)
prova = prova[, -1]
rm(train)

prop.table(table(prova$sex, prova$y))

prova$sex = as.factor(ifelse(prova$sex == 'male', 1, 0))
prova$y = as.factor(ifelse(prova$y == 'autism', 1, 0))



# Modello: Random Forest --------------------------------------------------

library(caret)
control <- trainControl(method='cv', 
                        number = 5)

metric <- "Accuracy"
set.seed(123)
mtry <- round(sqrt(ncol(prova) - 1)) # optimal number of columns

# tunegrid <- expand.grid(.mtry=mtry)
tunegrid <- expand.grid(.mtry=c(6, 9, 19)) # 21, 25

rf_default <- train(y ~. , 
                    data = prova, 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    tuneGrid = tunegrid,
                    trControl = control)
print(rf_default)
plot(rf_default)


# VARIABLE IMPORTANCE
baseline_accuracy <- rf_default$results$Accuracy

varImp(rf_default, scale = F)
var_imp = data.frame(varImp(rf_default, scale = F)$importance)
var_imp$variable = rownames(var_imp)
var_imp <- data.frame(var_imp[order(var_imp$Overall, decreasing = TRUE), ])

plot(var_imp[, 1], type = 'l', lwd = 2.5, col = 'steelblue')
grid()
abline(v = 50, col = 'red', lwd = 2, lty = 2)

top_variables = var_imp[1:10, 2]


# Proviamo a migliorare i parametri ---------------------------------------

prova_50 <- prova[, c(top_variables)]
prova_50$y <- prova$y

# Usiamo un control più ciotto
control50 <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')
# Mettiamo più valori di mtry
rf_default50 <- train(y ~. , 
                    data = prova_50, 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    #tuneGrid = tunegrid,
                    tuneLength = 20,
                    trControl = control50)
print(rf_default50)



# LOCO -------------------------------------------------------------------

top_variables

# PARTE 1: CONTROLLARE LA CORRELAZIONE TRA LE VARIABILI

prova_10 <- prova[, c(top_variables)]
prova_10_corr <- prova[, c(top_variables)]

library(corrplot)
corrplot(cor(prova_10_corr))

# COPPIE CHE FANNO STORIE:
# q3_8302, q1_8302
# q3_6402, q1_6402
# q3_6401, q1_6401
# q3_4022, q1_4022

# Eliminiamo q1_8302 perché è la meno importante
prova_10_corr <- prova_10_corr[, !(names(prova_10_corr) %in% c('q1_8302'))]
# , 'q1_6402', 'q1_6401', 'q1_4022'

prova_10_corr$y <- prova$y


control <- trainControl(method='cv', 
                        number = 5)

#tunegrid <- expand.grid(.mtry=9)
tunegrid <- expand.grid(.mtry=c(6, 9, 19, 21, 25))
rf_default10 <- train(y ~. , 
                    data = prova_10_corr, 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    tuneGrid = tunegrid,
                    #ntree = 1000,
                    trControl = control)
print(rf_default10)

set.seed(123)
variables_name = c()
accuracies = c()
new_acc = c()
for(j in 1:(ncol(prova_10_corr) - 1)){
  
  #variables_number <- c(variables, j)
  variables_name <- c(variables_name, colnames(prova_10_corr)[j])
  
  mtry <- round(sqrt(ncol(prova_10_corr) - 1)) # optimal number of columns
  tunegrid <- expand.grid(.mtry=9)
  mdl <- train(y ~. , 
               data = prova_10_corr[, -j], 
               method = 'rf', 
               metric = 'Accuracy', 
               tuneGrid = tunegrid, 
               trControl = control)
  
  new_acc <- c(new_acc, mdl$results$Accuracy)
  accuracies <- c(accuracies, baseline_accuracy - mdl$results$Accuracy)
}

data.frame(cbind('Missing Variable' = variables_name, 'Delta Accuracy' = accuracies), 'New Accuracy' = new_acc)



# Codice Raffaele per vedere il migliore ----------------------------------

set.seed(123)
control50 <- trainControl(method='repeatedcv', 
                          number=10, 
                          repeats=3,
                          search = 'random')
tunegrid <- expand.grid(.mtry=c(6, 9, 12, 15, 19, 21, 25))
rf_default10 <- train(y ~. , 
                      data = prova_10_corr, 
                      method = 'rf', 
                      metric = 'Accuracy', 
                      tuneGrid = tunegrid,
                      #ntree = 1000,
                      trControl = control50)
print(rf_default10)
baseline_accuracy <- rf_default10$results$Accuracy[2]


idx <- sample(1:600,size=120, replace=F)
prova_10_train <- prova_10_corr[-idx,]
prova_10_test <- prova_10_corr[idx,]
accuracy_test=c()
variables <- c()
accuracies <- c()

for(j in 1:(ncol(prova_10_corr) - 1)){
  
  variables <- c(variables, colnames(prova_10_corr)[j])
  
  mtry <- round(sqrt(ncol(prova_10_corr) - 1)) # optimal number of variables
  tunegrid <- expand.grid(.mtry=25) # <-- 25 era il numero che garantiva l'accuracy migliore
  mdl <- train(y ~. , 
               data = prova_10_train[, -j], 
               method = 'rf', 
               metric = 'Accuracy', 
               tuneGrid = tunegrid, 
               trControl = control)
  pred <- predict(mdl, newdata=prova_10_test)
  accuracy_test <- c(accuracy_test,mean(pred==prova_10_test$y))
  accuracies <- c(accuracies, baseline_accuracy - mdl$results$Accuracy)
}

data.frame(cbind('Missing Variable' = variables, 'Delta Accuracy' = round(accuracies, 5), 'accuracy'= round(accuracies+baseline_accuracy, 5), 'test_accuracy'= round(accuracy_test, 5)))

############

# Bene! Proviamo a togliere la variabile q1_6222
prova_10_corr <- prova_10_corr[, !(names(prova_10_corr) %in% c('q1_6222'))]
tunegrid <- expand.grid(.mtry=c(6, 9, 15, 19, 21, 25))
rf_default10 <- train(y ~. , 
                      data = prova_10_corr, 
                      method = 'rf', 
                      metric = 'Accuracy', 
                      tuneGrid = tunegrid,
                      #ntree = 50,
                      trControl = control)
print(rf_default10)
rf_default10



# Proviamo a togliere la 6 variabile --------------------------------------

prova_10_corr <- prova_10_corr[, -6]
tunegrid <- expand.grid(.mtry=c(3, 6))
rf_default10 <- train(y ~. , 
                      data = prova_10_corr, 
                      method = 'rf', 
                      metric = 'Accuracy', 
                      tuneGrid = tunegrid,
                      #ntree = 50,
                      trControl = control)
print(rf_default10)

# Correlazione tra variabili ----------------------------------------------


small_dataset <- prova_10[, -c(21)]

library(corrplot)
corrplot(cor(small_dataset))

cor(small_dataset[1:2]) # ce ne sono alcune troppo correlate. perciò ne dobbiamo togliere alcune... ma quali? Riguardiamo la variable importance
varImp(rf_default10, scale = F)


# small_dataset <- small_dataset[, -c('q1_8302', 'q1_4022')]
small_dataset <- small_dataset[, !(names(small_dataset) %in% c('q1_8302', 'q1_4022', 'q3_6401'))]
small_dataset$y <- prova_10$y


# LOCO di nuovo:
variables = c()
accuracies = c()
new_acc = c()
for(j in 1:(ncol(small_dataset) - 1)){
  
  variables <- c(variables, j)
  
  mtry <- round(sqrt(ncol(small_dataset) - 1)) # optimal number of columns
  tunegrid <- expand.grid(.mtry=mtry)
  mdl <- train(y ~. , 
               data = small_dataset[, -j], 
               method = 'rf', 
               metric = 'Accuracy', 
               tuneGrid = tunegrid, 
               trControl = control)
  
  new_acc <- c(new_acc, mdl$results$Accuracy)
  accuracies <- c(accuracies, baseline_accuracy - mdl$results$Accuracy)
}

data.frame(cbind('Missing Variable' = variables, 'Delta Accuracy' = accuracies), 'New Accuracy' = new_acc)


tunegrid <- expand.grid(.mtry=c(6, 9, 19, 21))
mdl_submission <- train(y ~. , 
             data = small_dataset[, -10], 
             method = 'rf', 
             metric = 'Accuracy', 
             tuneGrid = tunegrid, 
             trControl = control)

print(mdl_submission)

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

prova_test = cbind(test_info, q1_roi_test, q2_roi_test, q3_roi_test)
prova_test = prova_test[, -1]
# rm(test)

prova_test$sex = as.factor(ifelse(prova_test$sex == 'male', 1, 0))
# prova$y = as.factor(ifelse(prova$y == 'autism', 1, 0))

colnames(prova_10_corr)[1:58] # --> sono le nostre variabili, senza la y
prova_10_test <- prova_test[, colnames(prova_10_corr)[1:58]]
#prova_10 <- prova_10[, !(names(prova_10) %in% c('q1_5302'))]

id <- test$id
target = predict(rf_default10, newdata = prova_10_test)
target = ifelse(target == 1, 'autism', 'control')

preds_g11 = data.frame('id' = id, 'target' = target)
table(preds_g11$target)

write.csv(preds_g11, file = "preds_g11_10.csv", row.names = FALSE)
