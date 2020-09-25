#### Carrega pacotes ####

library(FSelector)
library(class)
library(dplyr)
library(psych)

#### Limpa workspace e consolo ####

rm(list = ls())   #limpa o workspace
cat("\014")       #limpa o console

#### Pré-processamento ####

# Carrega a base de dados de treino e de teste e declara NA como missing value####
set.seed(1)
setwd("C:\\Users\\rfsantos\\OneDrive - Mongeral Aegon\\Treinamentos\\BI Master\\DM\\Trabalho DM\\RStudio")
train = read.csv("horse.csv", header = TRUE, na.strings="NA")
test = read.csv("horseTest.csv", header = TRUE, na.strings="NA")

# Exclui colunas cp_data, hospital_number, lesion_2, lesion_3, nasogastric_reflux_ph
train = subset(train, select = -c(cp_data, hospital_number, lesion_2, lesion_3, nasogastric_reflux_ph))
test = subset(test, select = -c(cp_data, hospital_number, lesion_2, lesion_3, nasogastric_reflux_ph))

### Trata missing values 

# Trata valor "0" da coluna lesion_1 como NA
train$lesion_1 = na_if(train$lesion_1, "0")
test$lesion_1 = na_if(test$lesion_1, "0")

# Função Mode (moda)
Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

# Substitui missing values
train = train %>% select(outcome, lesion_1, everything())
test = test %>% select(outcome, lesion_1, everything())

for(i in 3:ncol(train))
  { if (class(train[,i]) == "character")
    { train[is.na(train[,i]), i] = Mode(train[,i])
      test[is.na(test[,i]), i] = Mode(train[,i])
      train[,i] = as.numeric(as.factor(train[,i]))
      test[,i] = as.numeric(as.factor(test[,i]))
  }
    else
    { train[is.na(train[,i]), i] = mean(train[,i], na.rm = TRUE)
      test[is.na(test[,i]), i] = mean(train[,i], na.rm = TRUE)
    }
}

train[is.na(train[,2]),2] = Mode(train[,2])
test[is.na(test[,2]),2] = Mode(train[,2])

str(train)

### Seleciona atributos de acordo com o ganho de informação
weights = information.gain(outcome~., train)
subset_w = cutoff.k(weights, 6)
train = subset(train, select = c(subset_w, "outcome"))
test = subset(test, select = c(subset_w, "outcome"))

### Redefine ordem das colunas
train = train %>% select(outcome, lesion_1, everything())
test = test %>% select(outcome, lesion_1, everything())

#### Treino ####

### ------------ k-NN ------------ ###
notInputs = 1
system.time(knn(train[,-notInputs],test[,-notInputs],cl=train$outcome, k = 3))
knn_model = knn(train[,-notInputs],test[,-notInputs],cl=train$outcome, k = 3)

# cl rótulo
# Faz o treinamento e as inferências na base de teste, pois#o teste já foi passado no treinamento. 
#a resposta do modelo já são as previsões

# Matriz de confusão
conf_matrix = table(knn_model, test$outcome)
conf_matrix
accuracy = 1 - mean(knn_model != test$outcome)
accuracy

#"métrica que avalia o nível de concordância de uma tarefa classificação"
kappa = confusionMatrix(cm)$overall[2];
kappa

