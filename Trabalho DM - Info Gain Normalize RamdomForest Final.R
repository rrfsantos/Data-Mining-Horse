# Carrega pacotes ####

library(FSelector)
library(class)
library(dplyr)
library(psych)
library(randomForest)
library(caret)

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

### Normalização
notInputs = 1
preprocessParams = preProcess(train[,-notInputs], method = "range")
train[,-notInputs] = predict(preprocessParams, train[,-notInputs])
test[,-notInputs] = predict(preprocessParams, test[-notInputs])

#### Treino ####

### ---------- Random Forest --------- ###

str(train)

# Treinamento
forest_model = randomForest(x=train[,2:(ncol(train))], y=as.factor(train[,"outcome"]), importance=TRUE, do.trace=15, ntree = 500 )

# Teste
predictionsForest = predict(forest_model, test)

# Matriz de confusão
m_conf = table(predictionsForest, test$outcome)
acuracy = 1 - mean(predictionsForest != test$outcome)
acuracy
kappa(m_conf)
