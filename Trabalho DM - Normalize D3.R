# Carrega pacotes ####

#install.packages("FSelector")
#install.packages("DMwR")
#install.packages("tree")
#install.packages("partykit")
library(class)
library(caret)
library(FSelector)
library(DMwR)
library(dummies)
library(tree)
library(rpart)
library(psych)

# Limpa workspace e consolo ####

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

# Trata valor "0" da coluna lesion_1 como NA
train$lesion_1 = na_if(train$lesion_1, "0")
test$lesion_1 = na_if(test$lesion_1, "0")

# Seleciona atributos de acordo com o ganho de informação
weights = information.gain(outcome~., train)
subset_w = cutoff.k(weights, 6)
train = subset(train, select = c(subset_w, "outcome"))
test = subset(test, select = c(subset_w, "outcome"))

summary(train)

# Trata missing values

train = train %>% select(lesion_1, everything())
test = test %>% select(lesion_1, everything())

Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

for(i in 2:ncol(train))
  { if (class(train[,i]) == "character")
    { train[is.na(train[,i]), i] = Mode(train[,i])
      test[is.na(test[,i]), i] = Mode(train[,i])
  }
    else
    { train[is.na(train[,i]), i] = median(train[,i], na.rm = TRUE)
      test[is.na(test[,i]), i] = median(train[,i], na.rm = TRUE)
    }
}

train[is.na(train[,1]),1] = Mode(train[,1])
test[is.na(test[,1]),1] = Mode(train[,1])


#Treino Decision Tree ####

# Treino
tree_model = tree(outcome ~ .,data = train)

# Teste
predictionsDtree = predict(tree_model, test)
length(predictionsDtree)
length(test)

# Matriz de Confusao
table(tree_predictions, test$outcome)
accuracy = 1 - mean(tree_predictions != test$outcome)
accuracy




summary(tree_model)
plot(tree_model)
text(tree_model)



#k-NN
#system.time(
knn_model = knn(train[,-i],test[,-i],cl=train$outcome, k = 2)

# cl rótulo
# Faz o treinamento e as inferências na base de teste, pois#o teste já foi passado no treinamento. 
#a resposta do modelo já são as previsões

table(knn_model, test$outcome)
accuracy = 1 - mean(knn_model != test$outcome)
accuracy
str(train)
