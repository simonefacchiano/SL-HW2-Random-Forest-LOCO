set.seed(123)


# Codice Raffaele ---------------------------------------------------------

idx <- sample(1:600,size=120, replace=F)
prova_10_train <- prova_10_corr[-idx,]
prova_10_test <- prova_10_corr[idx,]
accuracy_test=c()
variables <- c()
accuracies <- c()

for(j in 1:(ncol(prova_10_corr) - 1)){
  
  variables <- c(variables, colnames(prova_10_corr)[j])
  
  mtry <- round(sqrt(ncol(prova_10_corr) - 1)) # optimal number of variables
  tunegrid <- expand.grid(.mtry=mtry)
  mdl <- train(y ~. , 
               data = prova_10_train[, -j], 
               method = 'rf', 
               metric = 'Accuracy', 
               tuneGrid = tunegrid, 
               trControl = control)
  pred <- predict(mdl, newdata=prova_10_test)
  accuracy_test <- c(accuracy_test, round(mean(pred==prova_10_test$y), 3))
  accuracies <- c(accuracies, round(baseline_accuracy - mdl$results$Accuracy, 3))
}

data.frame(cbind('Missing Variable' = variables, 'Delta Accuracy' = accuracies, 'accuracy'= round(accuracies+baseline_accuracy, 3), 'test_accuracy'= accuracy_test))




# Codice Customizzato -----------------------------------------------------

numeri <- 1:10
combinazioni = data.frame(combn(numeri, 2))


all_comb <- list()

for(i in 1:ncol(combinazioni)){
  all_comb[i] <- list(combinazioni[, i])
}
 


# Creo il vettore con tutte le combinazioni di variabili:
variables <- colnames(prova_10_corr)[1:15]
combinazioni <- data.frame(combn(variables, 13))

all_comb <- list()
for(i in 1:ncol(combinazioni)){
  all_comb[i] <- list(combinazioni[, i])
}


idx <- sample(1:600,size=120, replace=F)
prova_10_train <- prova_10_corr[-idx,]
prova_10_test <- prova_10_corr[idx,]
accuracy_test=c()
variables <- c()
accuracies <- c()

for(j in 1:length(all_comb)){
  
  variables <- c(variables, j)
  
  new_data <- prova_10_train[, all_comb[[j]]]
  new_data$y <- prova_10_corr$y
  
  mtry <- round(sqrt(ncol(new_data) - 1)) # optimal number of variables
  tunegrid <- expand.grid(.mtry=mtry)
  
  mdl <- train(y ~. , 
               data = new_data, 
               method = 'rf', 
               metric = 'Accuracy', 
               tuneGrid = tunegrid, 
               trControl = control)
  pred <- predict(mdl, newdata=prova_10_test)
  accuracy_test <- c(accuracy_test, round(mean(pred==prova_10_test$y), 3))
  accuracies <- c(accuracies, round(baseline_accuracy - mdl$results$Accuracy, 3))
}

data.frame(cbind('Missing Variable' = variables, 'Delta Accuracy' = accuracies, 'accuracy'= round(accuracies+baseline_accuracy, 3), 'test_accuracy'= accuracy_test))





# LOCO - For Real ---------------------------------------------------------

prova_10 <- prova[, c(top_variables[1:10], 'y')] # <-- OUR TOP 10 VARIABLES

# STEP 1: 
idx <- createDataPartition(prova_10$y, p = 0.8, list = FALSE)
prova_10_train <- prova_10[-idx,] # 80%
prova_10_test <- prova_10[idx,] # 20%




# STEP 2:
mtry <- round(sqrt(ncol(prova_10_train) - 1)) # optimal number of columns
tunegrid <- expand.grid(.mtry=mtry)
control <- trainControl(method='cv', 
                        number = 5)

rf_default <- train(y ~. , 
                    data = prova_10_train, 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    tuneGrid = tunegrid,
                    trControl = control)
# print(rf_default)
f_hat <- factor(predict(rf_default, newdata = prova_10_train))
y_true <- factor(prova_10_train$y)
F1 = confusionMatrix(f_hat, y_true, mode = "everything", positive="1")$byClass[7]


# STEP 3:
# Si usa il test per ottenere M campioni bootstrap, e per ogni campione si fa il predict e si calcola l'F1 score. Si confronta F1_j con F1 per ognuno dei M campioni, e alla fine si prende la mediana.

rf_j <- train(y ~. ,
                    data = prova_10_train[, 1:10],
                    method = 'rf',
                    metric = 'Accuracy',
                    tuneGrid = tunegrid,
                    trControl = control)

f_hat_j <- factor(predict(rf_j, newdata = prova_10_test))
y_true <- factor(prova_10_test$y)
F1_j = confusionMatrix(f_hat_j, y_true, mode = "everything", positive="1")$byClass[7]
LOCO_score <- F1_j - F1

M <- 50
vec <- c()

for(m in 1:M){
  idx_boot <- sample(1:119, size = 119, replace = T)
  sample_boot <- prova_10_test[idx_boot, ]
  
  f_hat_j <- factor(predict(rf_j, newdata = sample_boot))
  y_true <- factor(sample_boot$y)
  
  F1_j = confusionMatrix(f_hat_j, y_true, mode = "everything", positive="1")$byClass[7]
  
  vec <- c(vec, F1_j)
  
}

LOCO_score <- c(mean(F1 - vec) - 2*sd(F1 - vec), mean(F1 - vec) + 2*sd(F1 - vec))




# LOCO... per davvero -----------------------------------------------------

LOCO_scores <- list()
LOCO_punctual <- c()

for(j in 1:(ncol(prova_10)-1)){
  rf_j <- train(y ~. ,
                data = prova_10_train[, -j],
                method = 'rf',
                metric = 'Accuracy',
                tuneGrid = tunegrid,
                trControl = control)
  
  vec <- c()
  
  for(m in 1:1000){
    idx_boot <- sample(1:119, size = 119, replace = T)
    sample_boot <- prova_10_test[idx_boot, ]
    
    f_hat_j <- factor(predict(rf_j, newdata = sample_boot))
    y_true <- factor(sample_boot$y)
    
    F1_j = confusionMatrix(f_hat_j, y_true, mode = "everything", positive="1")$byClass[7]
    
    vec <- c(vec, F1_j)
  }
  
  LOCO_scores[[j]] <- c(mean(F1-vec) - 2*sd(F1-vec), mean(F1-vec) + 2*sd(F1-vec))
  LOCO_punctual <- c(LOCO_punctual, median(F1-vec))
  
}

LOCO_scores
LOCO_punctual

plot(1, cex = 0, xlim = c(1, 10), ylim = c(-0.05, 1.05))
grid()
abline(h = 0, lty = 2)
for(i in 1:10){
  lines(x = c(i, i), y = c(unlist(LOCO_scores[i]) ), lwd = 2)
}

points(1:10, LOCO_punctual, pch = 20, col = 'red')

# SE E VICINO A 1, SIGNIFICA CHE F1_J E PICCOLA, E QUINDI LA VARIABILE ERA IMPORTANTE. PERCIò, UNO SCORE GRANDE IMPLICA UNA GRANDE IMPORTANZA.

points(1:10, var_imp[1:10, 1]/max(var_imp[1:10, 1]), pch = 20, col = 'forestgreen')
