# QUI HO SEMPLICEMENTE FATTO COSì:
# - RUNNATO UN PRIMO MODELLO CON TUTTE LE VARIABILI
# CONSIDERATO Q1, Q2, Q3, MIN E MAX
# FATTO LA VARIABLE IMPORTANCE E PLOTTATO IL RISULTATO
# NOTATO UN GOMITO A 50, E SELEZIONATO LE PRIME 50 VARIABILI
# USATO IL MODELLO CON LE 50 VARIABILI
# Niente selezione tramite LOCO


#     Statistical Learning Homework 2, group 11
# Susanna Bravi, Simone Facchiano, Raffaele Liguori



# 0 - Packages --------------------------------------------------------

library(readr)


# 1 - Initialization ------------------------------------------------------

setwd("~/Desktop/Data Science/SL/Homework 2")
train <- read_csv("train_hw03.csv")


# 2 - Preprocessing -------------------------------------------------------

# Problema: i valori (alcuni intorno allo 0, altri del tutto sballati)
hist(rowMeans(train[, -c(1:4)]), breaks = 20, prob = T) # ad esempio, qui la seconda riga ha media -3, mentre tutte le altre sono intorno allo 0
boxplot(rowMeans(train[, -c(1:4)]))

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
#colnames(sd_roi) = paste0('sd_', roi)
#colnames(mean_roi) = paste0('mean_', roi)
colnames(max_roi) = paste0('max_', roi)
colnames(min_roi) = paste0('min_', roi)


prova = cbind(train_info, q1_roi, q2_roi, q3_roi, min_roi, max_roi)
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

tunegrid <- expand.grid(.mtry=mtry)
#tunegrid <- expand.grid(.mtry=c(6, 9, 19, 21, 25))

rf_default <- train(y ~. , 
                    data = prova, 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    tuneGrid = tunegrid,
                    trControl = control)
print(rf_default)
plot(rf_default)


# VARIABLE IMPORTANCE
best_mtry <- as.numeric(rf_default$bestTune)
baseline_accuracy <- rf_default$results$Accuracy[rf_default$results$mtry == best_mtry]

varImp(rf_default, scale = F)
var_imp = data.frame(varImp(rf_default, scale = F)$importance)
var_imp$variable = rownames(var_imp)
var_imp <- data.frame(var_imp[order(var_imp$Overall, decreasing = TRUE), ])

var_imp$variable[var_imp$Overall > 1] # ora che abbiamo aggiunti i massimi e i minimi cambia un bel po'...

plot(var_imp[, 1], type = 'l', lwd = 2.5, col = 'steelblue')
grid()
abline(v = c(10, 50), col = 'red', lwd = 1.7, lty = 2)

top_variables = var_imp[1:50, 2]



# Nuovo modello con 50 variabili ------------------------------------------

prova_10_corr <- prova[, c(top_variables)]
prova_10_corr$y <- prova$y

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')

#tunegrid <- expand.grid(.mtry=9)
tunegrid <- expand.grid(.mtry=c(6, 7, 8, 9))
rf_default10 <- train(y ~. , 
                      data = prova_10_corr, 
                      method = 'rf', 
                      metric = 'Accuracy', 
                      tuneGrid = tunegrid,
                      trControl = control)
print(rf_default10)

# LOCO -------------------------------------------------------------------

top_variables

# PARTE 1: CONTROLLARE LA CORRELAIZONE TRA LE VARIABILI

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
                        number = 10)

#tunegrid <- expand.grid(.mtry=9)
tunegrid <- expand.grid(.mtry=c(6, 7, 8, 9))
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

idx <- sample(1:600,size=120, replace=F)
prova_10_train <- prova_10_corr[-idx,]
prova_10_test <- prova_10_corr[idx,]
accuracy_test=c()
variables <- c()
accuracies <- c()

for(j in 1:(ncol(prova_10_corr) - 1)){
  
  variables <- c(variables, colnames(prova_10_corr)[j])
  
  mtry <- round(sqrt(ncol(prova_10_corr) - 1)) # optimal number of variables
  tunegrid <- expand.grid(.mtry=19) # <-- 19 era il numero che garantiva l'accuracy migliore
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

data.frame(cbind('Missing Variable' = variables, 'Delta Accuracy' = round(accuracies, 3), 'accuracy'= round(accuracies+baseline_accuracy, 3), 'test_accuracy'= round(accuracy_test, 3)))

############

# Bene! Proviamo a togliere la variabile q2_8101
prova_10_corr <- prova_10_corr[, !(names(prova_10_corr) %in% c('q2_8101'))]
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




# LOCO per davvero --------------------------------------------------------

prova_10 <- prova[, c(top_variables[1:13], 'y')] # <-- OUR TOP VARIABLES + FLOP variables

# STEP 0: check for highly correlated variables
library(corrplot)
corrplot(cor(prova_10[, -14]))


high_cor_pairs <- which(abs(cor(prova_10[, -14])) > 0.6 & cor(prova_10[, -14]) < 0.99, arr.ind = TRUE)
discard_variables <- c()
for (i in 1:nrow(high_cor_pairs)) {
  variable1 <- colnames(cor(prova_10[, -14]))[high_cor_pairs[i, 1]]
  variable2 <- colnames(cor(prova_10[, -14]))[high_cor_pairs[i, 2]]
  
  ifelse(var_imp[variable1, 'Overall'] > var_imp[variable2, 'Overall'],
         discard_variables <- c(discard_variables, variable2),
         discard_variables <- c(discard_variables, variable1))
}

discard_variables <- unique(discard_variables)

library(dplyr)
LOCO_variables <- prova_10 %>%
  select(-one_of(discard_variables))

# STEP 1: partition in train e test
idx <- createDataPartition(LOCO_variables$y, p = 0.8, list = FALSE)
prova_10_train <- LOCO_variables[-idx,] # 80%
prova_10_test <- LOCO_variables[idx,] # 20%

# STEP 2: Run algorithm to compute estimate fˆ on first part D1
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
LOCO_scores <- list()
LOCO_punctual <- c()

for(j in 1:(ncol(LOCO_variables)-1)){
  rf_j <- train(y ~. ,
                data = prova_10_train[, -j],
                method = 'rf',
                metric = 'Accuracy',
                tuneGrid = tunegrid,
                trControl = control)
  
  vec <- c()
  
  for(m in 1:50){
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


#### GRAFICO FATTO BENE
 
library(ggplot2)

df <- data.frame(do.call(rbind, LOCO_scores))
LOCO_punctual_df <- data.frame(x = 1:10, y = LOCO_punctual)

x = seq(1, 10, length.out = 10)
y = seq(0, 1, length.out=10)
brks <- seq(0, 1, 0.1)
ggplot(data = df,
       mapping = aes(x=x,y=y))+
  geom_errorbar(alpha=1.5, linetype=1, size=0.8,
                ymin=df$X1,
                ymax=df$X2,
                color="#a3c4dc")+ 
  geom_point(x = LOCO_punctual_df$x, y = LOCO_punctual_df$y, col = "#0e668b")+
  coord_flip()+
  scale_y_continuous(breaks = brks, labels = brks)+
  scale_x_continuous(breaks = seq(0, 10, 1), labels = seq(0, 10, 1))+
  labs(title="Loco confidence intervals",
       x = "Variable Number",
       y = "LOCO score")+ 
  theme(text = element_text(family = "serif"), 
        plot.title = element_text(hjust=0.5),
        #plot.background=element_rect(fill="white"),
        panel.background=element_rect(fill="#fafafa"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank())




# Predict -----------------------------------------------------------------

test <- read_csv("test_hw03.csv")
head(test)


test_info <- test[, c(1:3)]
test_roi <- test[, -c(1:3)]

hist(rowMeans(test_roi))

# Scale
# 1)
test_roi_scale = data.frame(t(apply(test_roi, 1, scale)))

# 2)

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

# sd_roi_test <- as.data.frame(sapply(seq(1, ncol(test_roi_scale), 115), function(start) {
#   end <- start + 115 - 1
#   apply(test_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=sd)
# }))
# 
# mean_roi_test <- as.data.frame(sapply(seq(1, ncol(test_roi_scale), 115), function(start) {
#   end <- start + 115 - 1
#   apply(test_roi_scale[, start:end, drop = F], MARGIN = 1, FUN=mean)
# }))

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


# Ora posso inserirle nell'altra tabella:
colnames(q1_roi_test) = paste0('q1_', roi) # molto meglio
colnames(q2_roi_test) = paste0('q2_', roi)
colnames(q3_roi_test) = paste0('q3_', roi)
colnames(max_roi_test) = paste0('max_', roi)
colnames(min_roi_test) = paste0('min_', roi)


prova_test = cbind(test_info, q1_roi_test, q2_roi_test, q3_roi_test, min_roi_test, max_roi_test)
prova_test = prova_test[, -1]

prova_test$sex = as.factor(ifelse(prova_test$sex == 'male', 1, 0))

colnames(prova_10_corr)[1:50] # --> sono le nostre variabili, senza la y
prova_10_test <- prova_test[, colnames(prova_10_corr)[1:50]]

id <- test$id
target = predict(rf_default10, newdata = prova_10_test)
target = ifelse(target == 1, 'autism', 'control')

preds_g11 = data.frame('id' = id, 'target' = target)
table(preds_g11$target)

write.csv(preds_g11, file = "preds_g11_13.csv", row.names = FALSE)
