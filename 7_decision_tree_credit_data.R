#Exemplo curso machine learning usando R
setwd('C:\\Users\\guisc\\Dropbox\\Curso Udemy machine learning material')
base<-read.csv('credit-data.csv')

#elimina a vari�vel clientid
base$clientid <- NULL  

#substitui valors negativos pela m�dia usando ifelse
base$age <- ifelse(base$age<0,mean(base$age[base$age>0],na.rm=TRUE),base$age)

#adiciona valor m�dio nos valores NA na base de dados
base$age <- ifelse(is.na(base$age),mean(base$age[base$age>0],na.rm=TRUE),base$age)

#escalona todos os atributos entre si (income, age e loan), menos da coluna default. 
base[, 1:3] = scale(base[, 1:3])

#PRECISAMOS TRANSFORMAR TIPO DA VARI�VEL DO ARGUMENTO DE CLASSE EM FATOR
base$default <- factor(base$default, levels <- c(0,1))
#lembrar que 0 � o cliente que n�o pagou e 1 � o cliente que pagou

#divide a base de dados em subsets para treinamento
install.packages('caTools')
library(caTools)
set.seed(1)
divisao <- sample.split(base$default, SplitRatio = 0.75)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

#TANTO PARA NAIVE BAYES QUANTO DECISION TRESS � PRECISO ENCODE O ATRIBUTO CLASSE
base$default <- factor(base$default, levels = c(0,1))

#CRIANDO O CLASSIFICADOR VIA �RVORES DE DECIS�O
library(rpart)
classificador <- rpart(formula=default ~ income+age+loan, data<-base_treinamento)
print(classificador)
plot(classificador)
text(classificador)

library(rpart.plot)
rpart.plot(classificador)

#REALIZANDO PREVIS�ES COM BASE TESTE
previsoes <- predict(classificador, newdata = base_teste[-4])
previsoes #ele retorna a probabilidade da entrada ser 0 ou 1, ou seja, assumir um dos valores da classe
#aqui 0 � quem n�o paga o cr�dito e 1 � quem paga

#mas precisamos, para comparar acur�cia e outros usos apenas saber se � 0 ou 1. 
#para tal, adicionamos o atributo "type=class" na fun��o predict:
previsoes <- predict(classificador, newdata = base_teste[-4], type<-'class')

matriz_confusao <- table(base_teste$default,previsoes)
matriz_confusao

acertos <- matriz_confusao[1,1] + matriz_confusao[2,2]
erros <- matriz_confusao[1,2] + matriz_confusao[2,1]
qualidade_algoritmo <- acertos/(erros+acertos)
qualidade_algoritmo

#para analisar m�tricas da matriz de confus�o e outras regress�es, podemos usar pacote "caret"
install.packages('caret')
library('caret')
confusionMatrix(matriz_confusao) #retorna m�tricas do ajuste

#fazendo escalonamento + faltantes, acuracia = 0,97
#sem escalonamento, acur�cia = 0,97
#sem nenhum pr�-processamento, s� encode, acur�cia = 0,968















