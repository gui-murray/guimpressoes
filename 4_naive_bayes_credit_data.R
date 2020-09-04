#Exemplo curso machine learning usando R
setwd('C:\\Users\\guisc\\Dropbox\\Curso Udemy machine learning material')
base<-read.csv('credit-data.csv')

#elimina a variável clientid
base$clientid <- NULL  

#substitui valors negativos pela média usando ifelse
base$age <- ifelse(base$age<0,mean(base$age[base$age>0],na.rm=TRUE),base$age)

#adiciona valor médio nos valores NA na base de dados
base$age <- ifelse(is.na(base$age),mean(base$age[base$age>0],na.rm=TRUE),base$age)

#escalona todos os atributos entre si (income, age e loan), menos da coluna default. 
base[, 1:3] = scale(base[, 1:3])

#PRECISAMOS TRANSFORMAR TIPO DA VARIÁVEL DO ARGUMENTO DE CLASSE EM FATOR
base$default <- factor(base$default, levels <- c(0,1))
#lembrar que 0 é o cliente que não pagou e 1 é o cliente que pagou

#divide a base de dados em subsets para treinamento
#install.packages('caTools')
library(caTools)
set.seed(1)
divisao <- sample.split(base$default, SplitRatio = 0.75)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

#ATÉ AQUI SÓ TRATAMOS OS DADOS PARA USAR NO NAIVE BAYES

#abaixo começamos a criar a matriz probabilidades para o classificador naive Bayes

library('e1071')
#CRIAMOS A MATRIZ USANDO A base treinamento
classificador <- naiveBayes(x<-base_treinamento[,1:3], y<-base_treinamento$default)
print(classificador)

#agora vamos prever o resultado para o atributo de classe, default, para todos os dados da base_teste
previsoes <- predict(classificador, newdata = base_teste[-4]) #notar que excluímos o atributo coluna 4, que é o que queremos prever.
previsoes

#DEVEMOS COMPARAR AS PREVISÕES COM O REAL, MINIMIZAR ERRO?

#CRIAR UMA MATRIZ DE CONFUSÃO:
matriz_confusao <- table(base_teste[ ,4], previsoes)
matriz_confusao #é uma matriz que mostra intersecção atributo de classe com previsões

acertos <- matriz_confusao[1,1] + matriz_confusao[2,2]
erros <- matriz_confusao[1,2] + matriz_confusao[2,1]
qualidade_algoritmo <- acertos/(erros+acertos)
qualidade_algoritmo

#para analisar métricas da matriz de confusão e outras regressões, podemos usar pacote "caret"
#install.packages('caret')
library('caret')
confusionMatrix(matriz_confusao) #retorna métricas do ajuste









