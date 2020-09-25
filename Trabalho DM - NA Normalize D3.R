# Carrega pacotes ####

library(class)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(ggplot2)

# Limpa workspace e consolo ####

rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console

#### Pré-processamento ####

### Carrega a base de dados de treino e de teste e declara NA como missing value####
set.seed(1)
setwd("C:\\Users\\rfsantos\\OneDrive - Mongeral Aegon\\Treinamentos\\BI Master\\DM\\Trabalho DM\\RStudio")
train = read.csv("horse.csv", header = TRUE, na.strings="NA")
test = read.csv("horseTest.csv", header = TRUE, na.strings="NA")

### Exclui colunas cp_data, hospital_number, lesion_2, lesion_3, nasogastric_reflux_ph
train = subset(train, select = -c(cp_data, hospital_number, lesion_2, lesion_3, nasogastric_reflux_ph))
test = subset(test, select = -c(cp_data, hospital_number, lesion_2, lesion_3, nasogastric_reflux_ph))

### Trata missing values
# Trata valor "0" da coluna lesion_1 como NA
train$lesion_1 = na_if(train$lesion_1, "0")
test$lesion_1 = na_if(test$lesion_1, "0")
train = train %>% select(outcome, lesion_1, everything())
test = test %>% select(outcome, lesion_1, everything())

# Função Mode (moda)
Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

#str(test)
# Substitui missing values
for(i in 3:ncol(train))
  { if (class(train[,i]) == "character")
    { train[is.na(train[,i]), i] = Mode(train[,i])
      test[is.na(test[,i]), i] = Mode(train[,i])
      train[,i] = as.numeric(as.factor(train[,i]))
      test[,i] = as.numeric(as.factor(test[,i]))
    }
    else
    { train[is.na(train[,i]), i] = median(train[,i], na.rm = TRUE)
      test[is.na(test[,i]), i] = median(train[,i], na.rm = TRUE)
    }
  }

# Trata substitui NA pela moda em lesion_1
train[is.na(train[,2]),2] = Mode(train[,2])
test[is.na(test[,2]),2] = Mode(train[,2])

### Redefine ordem das colunas
train = train %>% select(outcome, lesion_1, everything())
test = test %>% select(outcome, lesion_1, everything())
train$outcome = as.factor(train$outcome)
test$outcome = as.factor(test$outcome)

### Normalização
notInputs = 1
preprocessParams = preProcess(train[,-notInputs], method = "pca")
train[,-notInputs] = predict(preprocessParams, train[,-notInputs])
test[,-notInputs] = predict(preprocessParams, test[-notInputs])

### Aplica PCA para seleção de atributos
pcatrain = train[,-notInputs]
pca <- princomp(pcatrain) #roda o pca na base
summary(pca) #resultados do pca (desvio padrão, variancia proporcional e proporção da variancia acumulada)
plot(pca)

#Pego somente os componentes principais que mantêm uma variância acumulada de 100% do total
vars = pca$sdev^2
vars = vars/sum(vars)
cumulativeVariance = cumsum(vars)
View(as.data.frame(cumulativeVariance)) #==> atributo 30: redução de 5 atributos
pcatrain = pca$scores[,1:18]
str(pcatrain)
pcatest = pca$scores[,1:18]
pcatrain = as.data.frame(pcatrain)
pcatest = as.data.frame(pcatest)
train = cbind(pcatrain, train$outcome)
test = cbind(pcatest, test$outcome)
colnames(train)[ncol(train)] = "outcome"
colnames(test)[ncol(test)] = "outcome"



#### Treino ####

### ---------- D - Tree --------- ###
# Treino
tree_model = rpart(outcome ~ ., train, method = "class")
rpart.rules(tree_model)
rpart.plot(tree_model)

# Teste
predictionsDtree = predict(tree_model, test, type = "class")
m_conf = ConfusionMatrix(predictionsDtree, test$outcome)
m_conf

acuracy = Accuracy(y_pred = predictionsDtree, y_true = test$outcome)
acuracy
kappa(m_conf)
