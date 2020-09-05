base<-read.csv('credit-data.csv')

#elimina a variável clientid
base$clientid <- NULL  

#substitui valores negativos pela média usando ifelse
base$age <- ifelse(base$age<0,mean(base$age[base$age>0],na.rm=TRUE),base$age)

#adiciona valor médio nos valores NA na base de dados
base$age <- ifelse(is.na(base$age),mean(base$age[base$age>0],na.rm=TRUE),base$age)

#escalona todos os atributos entre si (income, age e loan), menos da coluna default. 
base[, 1:3] = scale(base[, 1:3])

#PRECISAMOS TRANSFORMAR TIPO DA VARIÁVEL DO ARGUMENTO DE CLASSE EM FATOR
base$default <- factor(base$default, levels <- c(0,1))
#lembrar que 0 é o cliente que não pagou e 1 é o cliente que pagou

#divide a base de dados em subsets para treinamento.
#o kNN não SEPARA TREINAMENTO E TESTE, ELE USA TODO O BANCO DE DADOS E CALSSIFICA POR INSTÂNCIA OS NOVOS INPUTS
install.packages('caTools')
library(caTools)
set.seed(1)
divisao <- sample.split(base$default, SplitRatio = 0.75)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

#CRIANDO O CLASSIFICADOR DO kNN USANDO PACOTES DO R
install.packages("class")
library("class")

#no kNN a gente faz tudo ao mesmo tempo: treinamento e previsões
#train <- base_treinamento menos o atributo classe, test <- base_teste menos atributo classe,
#cl vai receber o atributo classe da base treinamento, e k é o número de vizinhos próximos
previsoes <- knn(train = base_treinamento[,-4], test = base_teste[,-4],
                 cl = base_treinamento[,4], k = 5)
#ele já gera as previsões direto, só analisar matriz d confusão agora
previsoes

confusao <- table(previsoes, base_teste$default)
confusao

library(caret)
confusionMatrix(confusao)

